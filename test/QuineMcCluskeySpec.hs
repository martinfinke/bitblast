module QuineMcCluskeySpec where

import SpecHelper
import QuineMcCluskey
import TruthTable(var)
import TruthTableSpec
import Formula(Formula(..), eval)
import FormulaSpec
import NormalForm(FormType(..), getFormula, ensureCanonical)
import NormalFormSpec
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set
import qualified Data.Matrix as M



instance Arbitrary QmcTerm where
    arbitrary = do
        (TenOrLess len) <- arbitrary
        str <- vectorOf len $ elements "10-"
        return $ fromString str

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

    describe "termToQmcTerm" $ do
        it "creates an empty QmcTerm when the length is 0" $ do
            show (termToQmcTerm 0 (And [])) `shouldBe` ""

    describe "QmcTerm Show instance" $ do
        it "shows a term with length 0 as the empty string" $ do
            show (termToQmcTerm 0 (And [])) `shouldBe` ""

        it "shows a term with length 1 as 1" $ do
            show (termToQmcTerm 1 (And [x0])) `shouldBe` "1"

        it "shows a term with length 2 as 10" $ do
            show (termToQmcTerm 2 (And [x1,Not x0])) `shouldBe` "10"

        it "shows a term with length 4 and a Dont-Care as 0-10" $ do
            show (termToQmcTerm 4 (And [Not x3,x1,Not x0])) `shouldBe` "0-10"
        it "is inverse to fromString" $ do
            property $ \qmcTerm -> (fromString . show) qmcTerm `shouldBe` qmcTerm

    describe "canonicalToQmcTerms" $ do
        it "converts a CNF to QmcTerms" $ do
            canonicalToQmcTerms testCnf `shouldBe` map fromString [
                "000", "001", "101", "110", "111"
                ]

    describe "numRelevantLiterals" $ do
        it "is 0 for an empty QmcTerm" $ do
            let empty = fromString ""
            numRelevantLiterals CNFType empty `shouldBe` 0
            numRelevantLiterals DNFType empty `shouldBe` 0

        it "calculates the correct number of literals" $ do
            let qmcTerms = canonicalToQmcTerms testCnf
            map (numRelevantLiterals CNFType) qmcTerms `shouldBe` [3,2,1,1,0]
            map (numRelevantLiterals DNFType) qmcTerms `shouldBe` [0,1,2,2,3]

        it "works correctly with don't care positions" $ do
            let qmcTerm = fromString "0-1-110----"
            numRelevantLiterals CNFType qmcTerm `shouldBe` 2
            numRelevantLiterals DNFType qmcTerm `shouldBe` 3

    describe "hammingDistance" $ do
        it "is zero for two empty terms" $ do
            hammingDistance (fromString "", fromString "") `shouldBe` 0

        it "is 1 for an empty term and one with 1 literal" $ do
            hammingDistance (fromString "-", fromString "1") `shouldBe` 1
            hammingDistance (fromString "1", fromString "-") `shouldBe` 1

        it "is 2 for an empty term and one with 2 literals" $ do
            hammingDistance (fromString "--", fromString "01") `shouldBe` 2
            hammingDistance (fromString "00", fromString "--") `shouldBe` 2

        it "is 2 for a term with 1 literal and another with 3 literals" $ do
            hammingDistance (fromString "-1-", fromString "110") `shouldBe` 2

        it "is 1 for a pair of terms with one different literal" $ do
            hammingDistance (fromString "000--01--", fromString "010--01--") `shouldBe` 1

    describe "dashesLineUp" $ do
        it "is False if dashes don't line up" $ do
            dashesLineUp (fromString "-1--") (fromString "----") `shouldBe` False

        it "is True if dashes line up" $ do
            dashesLineUp (fromString "-0---") (fromString "-1---") `shouldBe` True

        it "is True for empty terms" $ do
            dashesLineUp (fromString "") (fromString "") `shouldBe` True

    describe "dashWhenDifferent" $ do
        it "outputs a dash (Nothing) if elements are different" $ do
            dashWhenDifferent (Just True) (Just False) `shouldBe` Nothing
            dashWhenDifferent (Just False) (Just True) `shouldBe` Nothing

        it "outputs the Bool value if elements are the same" $ do
            dashWhenDifferent (Just True) (Just True) `shouldBe` Just True
            dashWhenDifferent (Just False) (Just False) `shouldBe` Just False

        it "throws an error when dashes don't align" $ do
            evaluate (dashWhenDifferent Nothing (Just True)) `shouldThrow` anyErrorCall
            evaluate (dashWhenDifferent (Just False) Nothing) `shouldThrow` anyErrorCall

    describe "mergeTerms" $ do
        it "doesn't merge two terms that are equal" $ do
            mergeTerms (fromString "101") (fromString "101") `shouldBe` Nothing

        it "doesn't merge two terms where the dashes don't line up" $ do
            mergeTerms (fromString "--1") (fromString "-11") `shouldBe` Nothing
            mergeTerms (fromString "01-") (fromString "-0-") `shouldBe` Nothing

        it "throws an error for terms with different lengths" $ do
            evaluate (mergeTerms (fromString "-") (fromString "-1")) `shouldThrow` anyErrorCall

        it "doesn't merge two terms that are different in 2 positions" $ do
            mergeTerms (fromString "-10") (fromString "-01") `shouldBe` Nothing

        it "doesn't merge two terms that are different in 3 positions" $ do
            mergeTerms (fromString "-100") (fromString "-011") `shouldBe` Nothing

        it "merges two terms that have exactly 1 difference" $ do
            mergeTerms (fromString "1") (fromString "0") `shouldBe` Just (fromString "-")
            mergeTerms (fromString "10") (fromString "11") `shouldBe` Just (fromString "1-")
            mergeTerms (fromString "1-0") (fromString "1-1") `shouldBe` Just (fromString "1--")
            mergeTerms (fromString "--10-110-1") (fromString "--10-100-1") `shouldBe` Just (fromString "--10-1-0-1")

    describe "neighbourKeys" $ do
        it "is the empty list for an empty input list" $ do
            neighbourKeys [] `shouldBe` []

        it "is the empty list for an input list with just one element" $ do
            neighbourKeys [5] `shouldBe` []

        it "is one pair for a list with two neighbours" $ do
            neighbourKeys [3,4] `shouldBe` [(3,4)]

        it "is the empty list for an input list with two numbers that aren't neighbours" $ do
            neighbourKeys [3,5] `shouldBe` []

        it "is two pairs for a list with three adjacent numbers" $ do
            neighbourKeys [6,7,8] `shouldBe` [(6,7), (7,8)]

        it "is two pairs for a list with two neighbouring pairs which are apart from each other" $ do
            neighbourKeys [3,4, 7,8] `shouldBe` [(3,4), (7,8)]

    describe "groupTerms" $ do
        it "is an empty group if there are no terms" $ do
            groupTerms CNFType [] `shouldBe` IntMap.fromList []

        it "groups one term into its number of literals" $ do
            let term = fromString "--100-101-001-"
            groupTerms CNFType [term] `shouldBe` IntMap.fromList [(5, [term])]
            groupTerms DNFType [term] `shouldBe` IntMap.fromList [(4, [term])]

        it "groups multiple terms correctly" $ do
            let terms@[t1,t2,t3,t4] = map fromString ["1-1111-", "1-1101-", "100101-", "0-1-110"]
            groupTerms CNFType terms `shouldBe` IntMap.fromList [(0,[t1]), (1,[t2]), (2,[t4]), (3,[t3])]
            groupTerms DNFType terms `shouldBe` IntMap.fromList [(3,[t3,t4]), (4,[t2]), (5,[t1])]

    describe "allPairsOfGroups" $ do
        it "is the empty list if there are no groups" $ do
            allPairsOfGroups (IntMap.fromList []) (0,1) `shouldBe` []

        it "is the empty list if there are groups, but the indices don't use them" $ do
            allPairsOfGroups testGroups (0,1) `shouldBe` []
            allPairsOfGroups testGroups (3,4) `shouldBe` []
    
        it "finds the correct neighbour pairs" $ do
            allPairsOfGroups testGroups (1,2) `shouldBe` [(fromString "-100", fromString "-110")]
            allPairsOfGroups testGroups (2,3) `shouldBe` [(fromString "-110", fromString "1110"), (fromString "-110", fromString "-111")]

    describe "possibleNeighbours" $ do
        it "determines the correct possible neighbours" $ do
            possibleNeighbours testGroups `shouldBe` [
                (fromString "-100", fromString "-110"),
                (fromString "-110", fromString "1110"),
                (fromString "-110", fromString "-111")
                ]

    describe "mergesOrNothing" $ do
        it "does the correct merges" $ do
            let neighbours = possibleNeighbours testGroups
            mergesOrNothing neighbours `shouldBe` map (fmap fromString) [Just "-1-0", Nothing, Just "-11-"]

    describe "termsUsedForMerging" $ do
        it "adds anything from the second list in positions where the first list is not Nothing" $ do
            let maybeMerges = [Just 1, Nothing, Nothing, Just 1, Just 1, Nothing] :: [Maybe Int]
            let neighbours = [(1,2), (-1,-2), (-10,-20), (10,20), (100,200), (-100,-200)]
            termsUsedForMerging maybeMerges neighbours `shouldBe` Set.fromList [1,2,10,20,100,200]

        it "works for testGroups" $ do
            let neighbours = possibleNeighbours testGroups
            let maybeMerges = mergesOrNothing neighbours
            termsUsedForMerging maybeMerges neighbours `shouldBe` Set.fromList (map fromString ["-100", "-110", "-110", "-111"])
            
    describe "formulaToPrimesFormula" $ do
        let [a,b,c,d,e,f,g] = map (Atom . var) [0,1,2,3,4,5,6]
        it "can't simplify a CNF-encoded 1-bit XOR" $ do
            let cnfXor = And [
                    Or [Not a, Not b, Not c],
                    Or [Not a, b, c],
                    Or [a, Not b, c],
                    Or [a, b, Not c]
                    ]
            formulaToPrimesFormula cnfXor `shouldBe` cnfXor

        it "simplifies a formula with two identical terms" $ do
            let term = Or [Not a, b, Not c]
            let cnf = And [term, term]
            let expected = And [term]
            formulaToPrimesFormula cnf `shouldBe` expected

        it "simplifies a formula with two terms that have redundancy" $ do
            let term1 = Or [Not a, b, Not c]
            let term2 = Or [a, b, Not c]
            let cnf = And [term1, term2]
            let expected = And [Or [b, Not c]]
            formulaToPrimesFormula cnf `shouldBe` expected

        it "produces a formula that, for a random assignment, has the same value as the original" $ do
            let term1 = Or [Not a, b, Not c]
            let term2 = Or [a, b, Not c]
            let formula = And [term1, term2]
            let primesFormula = formulaToPrimesFormula formula
            property $ \assignment -> eval assignment formula `shouldBe` eval assignment primesFormula

        it "the primesFormula of smallNestedFormula has the same values (for a random assignment) as the original" $ do
            let primesFormula = formulaToPrimesFormula smallNestedFormula
            property $ \assignment -> eval assignment smallNestedFormula `shouldBe` eval assignment primesFormula

        it "generates prime formulae that have the same value (for a random assignment) as the original CNF/DNF" $ do
            property $ \canonical assignment -> eval assignment (getFormula canonical) `shouldBe` eval assignment (formulaToPrimesFormula $ getFormula canonical)


    describe "qmcTermToTerm" $ do
        it "creates an empty term for an empty qmcTerm" $ do
            qmcTermToTerm CNFType (fromString "") `shouldBe` Or []

        it "creates a term with one element for a qmcTerm with one element" $ do
            qmcTermToTerm DNFType (fromString "0") `shouldBe` And [Not $ Atom (var 0)]
            qmcTermToTerm DNFType (fromString "1") `shouldBe` And [Atom (var 0)]


        it "creates a term with two elements for a qmcTerm with two elements" $ do
            qmcTermToTerm CNFType (fromString "01") `shouldBe` Or [Atom (var 0), Not $ Atom (var 1)]
            qmcTermToTerm CNFType (fromString "11") `shouldBe` Or [Atom (var 0), Atom (var 1)]

    describe "dropElement" $ do
        it "drops the first element" $ do
            dropElement 0 [0,1,2,3] `shouldBe` [1,2,3]

        it "drops an element in the middle" $ do
            dropElement 3 [0,1,2,3,4,5] `shouldBe` [0,1,2,4,5]

        it "drops the last element" $ do
            dropElement 3 [0,1,2,3] `shouldBe` [0,1,2]

    describe "isCoveredBy" $ do
        it "is True for terms that are covered" $ do
            (fromString "101" `isCoveredBy` fromString "101") `shouldBe` True
            (fromString "101" `isCoveredBy` fromString "10-") `shouldBe` True
            (fromString "101" `isCoveredBy` fromString "-0-") `shouldBe` True
            (fromString "101" `isCoveredBy` fromString "---") `shouldBe` True

        it "is False for terms that aren't covered" $ do
            (fromString "101" `isCoveredBy` fromString "00-") `shouldBe` False
            (fromString "101" `isCoveredBy` fromString "-1-") `shouldBe` False

    describe "dropMatrixRow" $ do
        let testMatrix = M.fromLists [
                [0,1,0,0,0],
                [1,0,1,0,1],
                [0,1,0,1,1]
                ]
        it "drops the first row correctly" $ do
            dropMatrixRow testMatrix 0 `shouldBe` M.fromLists [
                [1,0,1,0,1],
                [0,1,0,1,1]
                ]

        it "drops the last row correctly" $ do
            dropMatrixRow testMatrix 2 `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [1,0,1,0,1]
                ]

        it "drops a middle row correctly" $ do
            dropMatrixRow testMatrix 1 `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [0,1,0,1,1]
                ]

    describe "dropMatrixColumn" $ do
        let testMatrix = M.fromLists [
                [0,1,0,0,0],
                [1,0,1,0,1],
                [0,1,0,1,1]
                ]
        it "drops the first column correctly" $ do
            dropMatrixColumn testMatrix 0 `shouldBe` M.fromLists [
                [1,0,0,0],
                [0,1,0,1],
                [1,0,1,1]
                ]

        it "drops the last column correctly" $ do
            dropMatrixColumn testMatrix 4 `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,1,0],
                [0,1,0,1]
                ]

        it "drops a middle column correctly" $ do
            dropMatrixColumn testMatrix 2 `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,0,1],
                [0,1,1,1]
                ]
            

testGroups :: IntMap.IntMap [QmcTerm]
testGroups = IntMap.fromList [
                (1,[fromString "-100"]),
                (2,[fromString "-110"]),
                (3,[fromString "1110", fromString "-111"])
                ]