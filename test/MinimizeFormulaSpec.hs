module MinimizeFormulaSpec where

import SpecHelper
import MinimizeFormula
import TruthTable(var)
import TruthTableSpec
import Formula
import FormulaSpec
import NormalForm
import NormalFormSpec
import QmTerm
import QmTermSpec




spec :: Spec
spec = do
    let [x0,x1,x2,x3] = map (Atom . var) [0,1,2,3]
    let _0pos3neg = [Not x0, Not x1, Not x2]
    let _1pos2neg = [x0, Not x1, Not x2]
    let _2pos1neg_1diff = [x0, Not x1, x2]
    let _2pos1neg_2diff = [Not x0, x1, x2]
    let _3pos0neg = [x0, x1, x2]
    let testCnf = ensureCanonical $ And [
            Or _0pos3neg,
            Or _1pos2neg,
            Or _2pos1neg_1diff,
            Or _2pos1neg_2diff,
            Or _3pos0neg
            ]
    describe "valueForVariable" $ do
        it "is Nothing for an empty Formula" $ do
            property $ \variable -> valueForVariable (And []) variable `shouldBe` Nothing

        it "is Nothing for a Variable that doesn't appear in the Formula" $ do
            valueForVariable (And _0pos3neg) (var 3) `shouldBe` Nothing

        it "is True for a Variable that appears as a positive literal" $ do
            valueForVariable (Or _2pos1neg_1diff) (var 0) `shouldBe` Just True
        it "is False for a Variable that appears as a negative literal" $ do
            valueForVariable (And _1pos2neg) (var 1) `shouldBe` Just False
            valueForVariable (And _1pos2neg) (var 2) `shouldBe` Just False

    describe "termToQmTerm" $ do
        it "creates an empty QmTerm when the length is 0" $ do
            show (termToQmTerm 0 (And [])) `shouldBe` ""

    describe "canonicalToQmTerms" $ do
        it "converts a CNF to QmTerms" $ do
            canonicalToQmTerms testCnf `shouldBe` map fromString [
                "000", "100", "101", "011", "111"
                ]

    describe "qmTermToTerm" $ do
        it "creates an empty term for an empty qmTerm" $ do
            qmTermToTerm True (fromString "") `shouldBe` Or []

        it "creates a term with one element for a qmcTerm with one element" $ do
            qmTermToTerm False (fromString "0") `shouldBe` And [Not $ Atom (var 0)]
            qmTermToTerm False (fromString "1") `shouldBe` And [Atom (var 0)]

        it "creates a term with two elements for a qmcTerm with two elements" $ do
            qmTermToTerm True (fromString "10") `shouldBe` Or [Atom (var 0), Not $ Atom (var 1)]
            qmTermToTerm True (fromString "11") `shouldBe` Or [Atom (var 0), Atom (var 1)]

    describe "minimizeCanonical" $ do
        it "doesn't minimize XOR (because it's impossible)" $ do
            let xor = ensureCanonical $ Xor [x0,x1]
            let minimized = minimizeCanonical xor
            property $ \assignment -> eval assignment minimized `shouldBe` eval assignment (getFormula xor)

        it "minimizes a CNF with redundancies" $ do
            let redundant = ensureCanonical $ And [Or [x0, x1], Or [x0, Not x1]]
            minimizeCanonical redundant `shouldBe` And [Or [x0]]