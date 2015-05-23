module QuineMcCluskeyAdditionalSpec where

import SpecHelper
import QuineMcCluskey
import Data.Maybe(isJust, catMaybes)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set
import qualified Data.Matrix as M
import NormalForm(FormType(..))

spec :: Spec
spec = do
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

    describe "Minimization from Video" $ do
        -- Youtube: bkH0T3fArUI
        let l = True
        let o = False
        let primes = map fromString ["0--0", "-1-0", "001-", "010-", "-011", "1-11", "111-"]
        let terms = map fromString ["0010", "0101", "0110", "1011", "1100", "1110", "1111"]
        let initialState = emptyState terms primes

        it "creates the correct initial state" $ do
            let (_,_,matrix) = initialState
            matrix `shouldBe` M.fromLists [
                    [l,o,l,o,o,o,o],
                    [o,o,o,l,o,o,o],
                    [l,l,o,o,o,o,o],
                    [o,o,o,o,l,l,o],
                    [o,l,o,o,o,o,o],
                    [o,l,o,o,o,o,l],
                    [o,o,o,o,o,l,l]
                    ]

        it "removes a row correctly" $ do
            let (newTerms, newPrimes, newMatrix) = removeRow 3 initialState
            newPrimes `shouldBe` primes
            newTerms `shouldBe` map fromString ["0010", "0101", "0110", "1100", "1110", "1111"]
            newMatrix `shouldBe` M.fromLists [
                    [l,o,l,o,o,o,o],
                    [o,o,o,l,o,o,o],
                    [l,l,o,o,o,o,o],
                    [o,l,o,o,o,o,o],
                    [o,l,o,o,o,o,l],
                    [o,o,o,o,o,l,l]
                    ]

        it "removes a column correctly" $ do
            let (newTerms, newPrimes, newMatrix) = removeColumn 5 initialState
            newPrimes `shouldBe` map fromString ["0--0", "-1-0", "001-", "010-", "-011", "111-"]
            newTerms `shouldBe` terms
            newMatrix `shouldBe` M.fromLists [
                    [l,o,l,o,o,o],
                    [o,o,o,l,o,o],
                    [l,l,o,o,o,o],
                    [o,o,o,o,l,o],
                    [o,l,o,o,o,o],
                    [o,l,o,o,o,l],
                    [o,o,o,o,o,l]
                    ]

        it "finds the correct essential columns" $ do
            let (_,_,matrix) = initialState
            essentialColumns matrix `shouldBe` [3, 1]