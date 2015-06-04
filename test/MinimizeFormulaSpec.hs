module MinimizeFormulaSpec where

import SpecHelper
import MinimizeFormula
import Variable hiding(eval)
import Formula
import FormulaSpec
import NormalForm
import NormalFormSpec
import Qm
import qualified Data.Set as Set


spec :: Spec
spec = do
    let vars@[v0,v1,v2,v3] = generateVars 4
    let [x0,x1,x2,x3] = map Atom vars
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
            valueForVariable (And _0pos3neg) v3 `shouldBe` Nothing

        it "is True for a Variable that appears as a positive DNF literal" $ do
            valueForVariable (And _2pos1neg_1diff) v0 `shouldBe` Just True

        it "is False for a Variable that appears as a negative DNF literal" $ do
            valueForVariable (And _1pos2neg) v1 `shouldBe` Just False
            valueForVariable (And _1pos2neg) v2 `shouldBe` Just False


        it "is True for a Variable that appears as a negative CNF literal" $ do
            valueForVariable (Or _1pos2neg) v1 `shouldBe` Just True
            valueForVariable (Or _1pos2neg) v2 `shouldBe` Just True

    describe "canonicalToBitVectors" $ do
        it "converts a CNF to QmTerms" $ do
            let varSet = variableSet (getFormula testCnf)
            Set.fromList (canonicalToBitVectors varSet testCnf) `shouldBe` Set.fromList (map (getTerm . fromString) [
                "0111", "0110", "0010", "0001", "0000"
                ])

    describe "minimizeCanonical" $ do
        it "doesn't minimize XOR (because it's impossible)" $ do
            let xor = ensureCanonical $ Xor [x0,x1]
            let minimized = minimizeCanonical xor
            property $ \assignment -> 
                let a1 = expandOrReduce False (variableSet (getFormula xor)) assignment
                    a2 = expandOrReduce False (variableSet minimized) assignment
                in a2 `isModelOf` minimized `shouldBe` a1 `isModelOf` getFormula xor

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
            let varSet = variableSet (getFormula canonical)
            let canonicalBitVectors = canonicalToBitVectors varSet canonical
            canonicalBitVectors `shouldBe` map (getTerm . fromString) [
                    "000",
                    "010",
                    "100"
                    ]
            let minimumCover = qm canonicalBitVectors [] []
            minimumCover `shouldBe` map fromString ["0-0", "-00"]
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
            minimized `shouldBe` Or [And [x0, x1], And [x0, x3]]


        it "minimizes any canonical to a formula that has the same value (for a random assignment)" $ do
            property $ \canonical assignment ->
                let minimized = minimizeCanonical canonical
                    a1 = expandOrReduce False (variableSet (getFormula canonical)) assignment
                    a2 = expandOrReduce False (variableSet minimized) assignment
                in a2 `isModelOf` minimized `shouldBe` a1 `isModelOf` getFormula canonical

    describe "convertDashes" $ do
        it "converts dashes to 0 and 1" $ do
            convertDashes (fromString "1-0") `shouldBe` map (getTerm . fromString) ["110", "100"]
            convertDashes (fromString "1--") `shouldBe` map (getTerm . fromString) ["111", "101", "110", "100"]

    describe "packTerm" $ do
        it "compresses a Formula term into a QmTerm" $ do
            let formulaTerm = And [x0, x1, Not x3]
            let varSet = Set.fromList [v0,v1,v3]
            packTerm varSet formulaTerm `shouldBe` fromString "011"
        it "compresses a single literal" $ do
            let formulaTerm = Or [Not x2]
            let varSet = Set.fromList [v2]
            packTerm varSet formulaTerm `shouldBe` fromString "1"

    describe "unpackTerm" $ do
        let varSet = Set.fromList vars
        it "decompresses a QmTerm with one literal into a Formula" $ do
            let varSet = Set.fromList [v2]
            unpackTerm True varSet (fromString "1") `shouldBe` Or [Not x2]
        it "ignores dashes" $ do
            unpackTerm False varSet (fromString "-10-") `shouldBe` And [Not x1, x2]
        it "is inverse to packTerm" $ do
            let test cnfMode formula variables = unpackTerm cnfMode (Set.fromList variables) (packTerm (Set.fromList variables) formula) `shouldBe` formula
            test True (Or [x0,x1,x3]) [v0,v1,v3]
            test False (And [x1,Not x3]) [v0,v1,v3]
            test True (Or [Not x1,Not x3]) [v0,v1,v2,v3]
            test False (And [Not x0,x1,Not x3]) [v0,v1,v3]

        it "creates an empty term for an empty qmTerm" $ do
            unpackTerm True Set.empty (fromString "") `shouldBe` Or []

        it "creates a term with one element for a qmcTerm with one element" $ do
            unpackTerm False varSet (fromString "---0") `shouldBe` And [Not x0]
            unpackTerm False varSet (fromString "---1") `shouldBe` And [x0]

        it "creates a term with two elements for a qmcTerm with two elements" $ do
            unpackTerm True varSet (fromString "--01") `shouldBe` Or [Not x0, x1]
            unpackTerm True varSet (fromString "--00") `shouldBe` Or [x0, x1]

        it "creates a term with three elements for a qmcTerm with three elements" $ do
            unpackTerm False varSet (fromString "1-01") `shouldBe` And [x0, Not x1, x3]