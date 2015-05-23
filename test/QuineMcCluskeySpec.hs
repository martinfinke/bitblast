module QuineMcCluskeySpec where

import SpecHelper
import QuineMcCluskey
import TruthTable(var)
import Formula(Formula(..))
import NormalForm(FormType(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set
import Data.Maybe(isJust, catMaybes)


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
    let testCnf = And [
            Or _0pos3neg,
            Or _1pos2neg,
            Or _2pos1neg_1diff,
            Or _2pos1neg_2diff,
            Or _3pos0neg
            ]

    describe "valueForVariableIndex" $ do
        it "is Nothing for an empty Formula" $ do
            property $ \variableIndex -> valueForVariableIndex (And []) variableIndex `shouldBe` Nothing

        it "is Nothing for a Variable that doesn't appear in the Formula" $ do
            valueForVariableIndex (And _0pos3neg) 3 `shouldBe` Nothing

        it "is True for a Variable that appears as a positive literal" $ do
            valueForVariableIndex (Or _2pos1neg_1diff) 0 `shouldBe` Just True
        it "is False for a Variable that appears as a negative literal" $ do
            valueForVariableIndex (And _1pos2neg) 1 `shouldBe` Just False
            valueForVariableIndex (And _1pos2neg) 2 `shouldBe` Just False

    describe "termToQmcTerm" $ do
        it "creates an empty QmcTerm when the length is 0" $ do
            termToQmcTerm 0 (And []) `shouldBe` QmcTerm V.empty

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

    describe "formulaToQmcTerms" $ do
        it "converts an empty Formula to the empty list" $ do
            map show (formulaToQmcTerms (Equiv [])) `shouldBe` []

        it "converts a CNF to QmcTerms" $ do
            map show (formulaToQmcTerms testCnf) `shouldBe` [
                "000", "001", "101", "110", "111"
                ]

    describe "numRelevantLiterals" $ do
        it "is 0 for an empty QmcTerm" $ do
            let empty = fromString ""
            numRelevantLiterals CNFType empty `shouldBe` 0
            numRelevantLiterals DNFType empty `shouldBe` 0

        it "calculates the correct number of literals" $ do
            let qmcTerms = formulaToQmcTerms testCnf
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

    describe "Example Terms 1" $ do
        -- Youtube: pQ3MfzqGlrc
        let terms@[t2,t4,t5,t6,t7] = map fromString ["010", "100", "101", "110", "111"]
        let initialGroups = groupTerms DNFType terms
        let afterFirstStep@[t2_6,t4_5,t4_6,t5_7,t6_7] = catMaybes $ map (uncurry mergeTerms) [(t2,t6), (t4,t5), (t4,t6), (t5,t7), (t6,t7)]
        let primesAfterFirstStep = fst $ qmcStep DNFType terms
        let groupsAfterFirstStep = groupTerms DNFType afterFirstStep
        let afterSecondStep = catMaybes $ map (uncurry mergeTerms) [(t4_5,t6_7), (t4_6,t5_7)]
        let primesAfterSecondStep = fst $ qmcStep DNFType afterFirstStep
        let groupsAfterSecondStep = groupTerms DNFType afterSecondStep

        let primesAfterThirdStep = fst $ qmcStep DNFType afterSecondStep
        let expectedPrimes = map fromString ["-10", "1--"]

        it "groups the initial terms correctly" $ do
            initialGroups `shouldBe` IntMap.fromList [
                    (1,[t2,t4]),
                    (2,[t5,t6]),
                    (3,[t7])
                    ]

        it "groups correctly after the first step" $ do
            groupsAfterFirstStep `shouldBe` IntMap.fromList [
                    (1,[t2_6,t4_5,t4_6]),
                    (2,[t5_7,t6_7])
                    ]

        it "groups correctly after the second step" $ do
            groupsAfterSecondStep `shouldBe` IntMap.fromList [
                    (1,afterSecondStep)
                    ]

        it "merges the initial terms correctly" $ do
            afterFirstStep `shouldBe` map fromString ["-10", "10-", "1-0", "1-1", "11-"]

        it "merges correctly in the second step" $ do
            afterSecondStep `shouldBe` map fromString ["1--", "1--"]

        it "doesn't find primes in the first step" $ do
            primesAfterFirstStep `shouldBe` []

        it "finds one prime in the second step" $ do
            primesAfterSecondStep `shouldBe` [fromString "-10"]

        it "finds one prime (actually, two identical ones) in the third step" $ do
            primesAfterThirdStep `shouldBe` [fromString "1--"]

        it "finds the expected primes in the end" $ do
            qmcPrimes DNFType terms `shouldBe` expectedPrimes

    describe "Example Terms 2" $ do
        -- Youtube: pQ3MfzqGlrc
        let terms@[t0,t1,t2,t8,t3,t5,t10,t7,t14,t15] = map fromString ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]
        let initialGroups = groupTerms DNFType terms
        let afterFirstStep@[t0_1,t0_2,t0_8,t1_3,t1_5,t2_3,t2_10,t8_10,t3_7,t5_7,t10_14,t7_15,t14_15] = catMaybes $ map (uncurry mergeTerms) [(t0,t1), (t0,t2), (t0,t8), (t1,t3), (t1,t5), (t2,t3), (t2,t10), (t8,t10), (t3,t7), (t5,t7), (t10,t14), (t7,t15), (t14,t15)]
        let groupsAfterFirstStep = groupTerms DNFType afterFirstStep
        let afterSecondStep@[t0_1_2_3,t0_2_1_3,t0_2_8_10,t0_8_2_10,t1_3_5_7,t1_5_3_7] = catMaybes $ map (uncurry mergeTerms) [(t0_1,t2_3), (t0_2,t1_3), (t0_2,t8_10), (t0_8,t2_10), (t1_3,t5_7), (t1_5,t3_7)]
        let groupsAfterSecondStep = groupTerms DNFType afterSecondStep

        let expectedPrimes = [t10_14, t7_15, t14_15, t0_1_2_3, t0_2_1_3, t0_2_8_10, t0_8_2_10, t1_3_5_7, t1_5_3_7] -- not unique

        it "groups the initial terms correctly" $ do
            initialGroups `shouldBe` IntMap.fromList [
                    (0,[t0]),
                    (1,[t1,t2,t8]),
                    (2,[t3,t5,t10]),
                    (3,[t7,t14]),
                    (4,[t15])
                    ]

        it "groups correctly after the first step" $ do
            groupsAfterFirstStep `shouldBe` IntMap.fromList [
                    (0,[t0_1,t0_2,t0_8]),
                    (1,[t1_3,t1_5,t2_3,t2_10,t8_10]),
                    (2,[t3_7,t5_7,t10_14]),
                    (3,[t7_15,t14_15])
                    ]

        it "groups correctly after the second step" $ do
            groupsAfterSecondStep `shouldBe` IntMap.fromList [
                    (0,[t0_1_2_3,t0_2_1_3,t0_2_8_10,t0_8_2_10]),
                    (1,[t1_3_5_7,t1_5_3_7])
                    ]

        it "merges the initial terms correctly" $ do
            afterFirstStep `shouldBe` map fromString ["000-", "00-0", "-000", "00-1", "0-01", "001-", "-010", "10-0", "0-11", "01-1", "1-10", "-111", "111-"]

        it "merges correctly in the second step" $ do
            afterSecondStep `shouldBe` map fromString ["00--", "00--", "-0-0", "-0-0", "0--1", "0--1"]

        it "finds the correct primes" $ do
            Set.fromList (qmcPrimes DNFType terms) `shouldBe` Set.fromList expectedPrimes

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
            

testGroups :: IntMap.IntMap [QmcTerm]
testGroups = IntMap.fromList [
                (1,[fromString "-100"]),
                (2,[fromString "-110"]),
                (3,[fromString "1110", fromString "-111"])
                ]