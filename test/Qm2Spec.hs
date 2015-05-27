module Qm2Spec where

import SpecHelper
import Qm2
import Data.Word(Word64)
import qualified Data.Set as Set

instance Arbitrary QmTerm where
    arbitrary = do
        term <- arbitrary :: Gen Word64
        mask <- arbitrary :: Gen Word64
        return $ QmTerm (term, mask)

spec :: Spec
spec = do
    describe "QmTerm Show instance" $ do
        it "is inverse to fromString" $ do
            property $ \qmTerm -> (fromString . show) qmTerm `shouldBe` qmTerm

    describe "merge" $ do
        it "behaves as the python version" $ do
            merge (fromString "0") (fromString "0") `shouldBe` Just (fromString "0")
            merge (fromString "0") (fromString "1") `shouldBe` Just (fromString "-")
            merge (fromString "0") (fromString "-") `shouldBe` Nothing
            merge (fromString "00") (fromString "01") `shouldBe` Just (fromString "0-")
            merge (fromString "001") (fromString "011") `shouldBe` Just (fromString "0-1")
            merge (fromString "001") (fromString "01-") `shouldBe` Nothing
            merge (fromString "-0-") (fromString "---") `shouldBe` Nothing

    describe "bitcount" $ do
        it "behaves as the python version" $ do
            bitcount True (getTerm $ fromString "") `shouldBe` 0
            bitcount True (getTerm $ fromString "0") `shouldBe` 0
            bitcount True (getTerm $ fromString "1") `shouldBe` 1
            bitcount True (getTerm $ fromString "10") `shouldBe` 1
            bitcount True (getTerm $ fromString "010") `shouldBe` 1
            bitcount True (getTerm $ fromString "0110") `shouldBe` 2

    describe "fromString" $ do
        it "converts 0 to 0" $ do
            getTerm (fromString "0") `shouldBe` 0
        it "converts 1 to 1" $
            getTerm (fromString "1") `shouldBe` 1
        it "converts 10 to 2" $
            getTerm (fromString "10") `shouldBe` 2
        it "converts 11 to 3" $
            getTerm (fromString "11") `shouldBe` 3

    describe "compute_primes" $ do
        it "behaves as the python version" $ do
            let test cubes vars expected = compute_primes False (Set.fromList cubes) vars `shouldBe` Set.fromList (map QmTerm expected)

            test [] 0 []
            test [0] 0 [(0, 0)]

            test [] 1 []
            test [0] 1 [(0, 0)]
            test [1] 1 [(1, 0)]
            test [2] 1 [(2, 0)]

            test [] 2 []
            test [0] 2 [(0, 0)]
            test [1] 2 [(1, 0)]
            test [2] 2 [(2, 0)]
            test [3] 2 [(3, 0)]
            test [0,1] 2 [(0, 1)]
            test [1,2] 2 [(2, 0), (1, 0)]
            test [2,3] 2 [(2, 1)]
            test [3,4] 2 [(3, 0), (4, 0)]
            test [0,1,3] 2 [(0, 1), (1, 2)]
            test [1,2,4] 2 [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] 2 [(2, 4), (2, 1)]
            test [3,4,8] 2 [(3, 0), (8, 0), (4, 0)]

            test [] 3 []
            test [0] 3 [(0, 0)]
            test [1] 3 [(1, 0)]
            test [2] 3 [(2, 0)]
            test [3] 3 [(3, 0)]
            test [0,1] 3 [(0, 1)]
            test [1,2] 3 [(2, 0), (1, 0)]
            test [2,3] 3 [(2, 1)]
            test [3,4] 3 [(3, 0), (4, 0)]
            test [0,1,3] 3 [(0, 1), (1, 2)]
            test [1,2,4] 3 [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] 3 [(2, 4), (2, 1)]
            test [3,4,8] 3 [(3, 0), (8, 0), (4, 0)]
            test [0,1,3] 3 [(0, 1), (1, 2)]
            test [1,2,4,9] 3 [(2, 0), (1, 8), (4, 0)]
            test [2,3,6,13] 3 [(2, 1), (2, 4), (13, 0)]
            test [3,4,8,12] 3 [(3, 0), (8, 4), (4, 8)]




