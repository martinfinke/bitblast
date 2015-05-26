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
import Qm
import Debug.Trace(traceShow)

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

        it "is True for a Variable that appears as a positive DNF literal" $ do
            valueForVariable (And _2pos1neg_1diff) (var 0) `shouldBe` Just True

        it "is False for a Variable that appears as a negative DNF literal" $ do
            valueForVariable (And _1pos2neg) (var 1) `shouldBe` Just False
            valueForVariable (And _1pos2neg) (var 2) `shouldBe` Just False


        it "is True for a Variable that appears as a negative CNF literal" $ do
            valueForVariable (Or _1pos2neg) (var 1) `shouldBe` Just True
            valueForVariable (Or _1pos2neg) (var 2) `shouldBe` Just True

    describe "canonicalToQmTerms" $ do
        it "converts a CNF to QmTerms" $ do
            canonicalToQmTerms testCnf `shouldBe` map fromString [
                "111", "011", "010", "100", "000"
                ]

    describe "qmTermToTerm" $ do
        it "creates an empty term for an empty qmTerm" $ do
            qmTermToTerm True (fromString "") `shouldBe` Or []

        it "creates a term with one element for a qmcTerm with one element" $ do
            qmTermToTerm False (fromString "0") `shouldBe` And [Not $ Atom (var 0)]
            qmTermToTerm False (fromString "1") `shouldBe` And [Atom (var 0)]

        it "creates a term with two elements for a qmcTerm with two elements" $ do
            qmTermToTerm True (fromString "10") `shouldBe` Or [Not $ Atom (var 0), Atom (var 1)]
            qmTermToTerm True (fromString "00") `shouldBe` Or [Atom (var 0), Atom (var 1)]

    describe "minimizeCanonical" $ do
        it "doesn't minimize XOR (because it's impossible)" $ do
            let xor = ensureCanonical $ Xor [x0,x1]
            let minimized = minimizeCanonical xor
            property $ \assignment -> eval assignment minimized `shouldBe` eval assignment (getFormula xor)

        it "minimizes a CNF with redundancies" $ do
            let redundant = ensureCanonical $ And [Or [x0, x1], Or [x0, Not x1]]
            minimizeCanonical redundant `shouldBe` And [Or [x0]]

        it "doesn't minimize an empty Or (=False)" $ do
            let emptyOr = ensureCanonical (Or [])
            getFormula emptyOr `shouldBe` Or []
            minimizeCanonical emptyOr `shouldBe` Or []

        it "doesn't minimize an empty And (=True)" $ do
            let emptyAnd = ensureCanonical (And [])
            getFormula emptyAnd `shouldBe` And []
            minimizeCanonical emptyAnd `shouldBe` And []

        it "creates the correct minimized CNF" $ do
            -- ((0 || 1 || 3) && (0 || -1 || 3) && (0 || 1 || -3))
            let example = And [
                    Or [x0, x1, x3],
                    Or [x0, Not x1, x3],
                    Or [x0, x1, Not x3]
                    ]
            let canonical = ensureCanonical example
            getFormula canonical `shouldBe` example

            let canonicalTerms = canonicalToQmTerms canonical
            canonicalTerms `shouldBe` map fromString ["0010", "0000", "0110", "0100", "0011", "0001"]
            let minimumCover = qmCnf (map s2b canonicalTerms) [] []
            minimumCover `shouldBe` map fromString ["--0", "0--"]
            let minimized = minimizeCanonical canonical
            minimized `shouldBe` And [Or [x0, x3], Or [x0, x1]]

        it "creates a correct minimized DNF" $ do
            -- (0 && 1 && 3) || (0 && -1 && 3) || (0 && 1 && -3)
            let example = Or [
                    And [x0, x1, x3],
                    And [x0, Not x1, x3],
                    And [x0, x1, Not x3]
                    ]
            let canonical = ensureCanonical example
            getFormula canonical `shouldBe` example
            let minimized = minimizeCanonical canonical
            minimized `shouldBe` Or [And [x0, x3], And [x0, x1]]


        it "minimizes any canonical to a formula that has the same value (for a random assignment)" $ do
            property $ \canonical assignment ->
                let minimized = minimizeCanonical canonical
                in eval assignment minimized `shouldBe` eval assignment (getFormula canonical)

    describe "termToQmTerm" $ do
        it "converts dashes to 0 and 1" $ do
            termToQmTerm 1 (Or []) `shouldBe` map fromString ["1", "0"]
            termToQmTerm 2 (And [x0]) `shouldBe` map fromString ["11", "10"]
            termToQmTerm 4 (And [x0, Not x2, x3]) `shouldBe` map fromString ["1101", "1001"]