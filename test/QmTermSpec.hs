module QmTermSpec where

import SpecHelper
import QmTerm


instance Arbitrary QmTerm where
    arbitrary = do
        (TenOrLess len) <- arbitrary
        str <- vectorOf len $ elements "10-"
        return $ fromString str

spec :: Spec
spec = do
    describe "QmTerm Show instance" $ do
        it "is inverse to fromString" $ do
            property $ \qmTerm -> (fromString . show) qmTerm `shouldBe` qmTerm

    describe "bitcount" $ do
            it "behaves as the python version" $ do
                bitcount True (fromString "") `shouldBe` 0
                bitcount True (fromString "0") `shouldBe` 0
                bitcount True (fromString "1") `shouldBe` 1
                bitcount True (fromString "10") `shouldBe` 1
                bitcount True (fromString "010") `shouldBe` 1
                bitcount True (fromString "0110") `shouldBe` 2

    describe "b2s" $ do
        it "behaves as the python version" $ do
            b2s 0 0 `shouldBe` fromString ""
            b2s 0 1 `shouldBe` fromString "0"
            b2s 0 2 `shouldBe` fromString "00"
            b2s 0 3 `shouldBe` fromString "000"
            b2s 0 4 `shouldBe` fromString "0000"

            b2s 1 0 `shouldBe` fromString ""
            b2s 1 1 `shouldBe` fromString "1"
            b2s 1 2 `shouldBe` fromString "01"
            b2s 1 3 `shouldBe` fromString "001"
            b2s 1 4 `shouldBe` fromString "0001"

            b2s 2 0 `shouldBe` fromString ""
            b2s 2 1 `shouldBe` fromString "0"
            b2s 2 2 `shouldBe` fromString "10"
            b2s 2 3 `shouldBe` fromString "010"
            b2s 2 4 `shouldBe` fromString "0010"

            b2s 3 0 `shouldBe` fromString ""
            b2s 3 1 `shouldBe` fromString "1"
            b2s 3 2 `shouldBe` fromString "11"
            b2s 3 3 `shouldBe` fromString "011"
            b2s 3 4 `shouldBe` fromString "0011"

    describe "s2b" $ do
        it "converts 0 to 0 and 1 to 1" $ do
            s2b (fromString "0") `shouldBe` 0
            s2b (fromString "1") `shouldBe` 1

        it "converts terms with more than one digit" $ do
            s2b (fromString "00") `shouldBe` 0
            s2b (fromString "01") `shouldBe` 1
            s2b (fromString "10") `shouldBe` 2
            s2b (fromString "11") `shouldBe` 3
            s2b (fromString "100") `shouldBe` 4

        it "is inverse to b2s" $ do
            let numVars = 7 -- enough to hold any random i <= 100
            property $ \(OneHundredOrLess i) -> s2b (b2s i numVars) `shouldBe` i

    describe "merge" $ do
        it "behaves as the python version" $ do
            merge (fromString "") (fromString "") `shouldBe` Just (fromString "")
            merge (fromString "") (fromString "1") `shouldBe` Just (fromString "")
            merge (fromString "") (fromString "11") `shouldBe` Just (fromString "")
            merge (fromString "1") (fromString "") `shouldBe` Just (fromString "")
            merge (fromString "11") (fromString "") `shouldBe` Just (fromString "")

            merge (fromString "0") (fromString "0") `shouldBe` Just (fromString "0")
            merge (fromString "0") (fromString "1") `shouldBe` Just (fromString "-")
            merge (fromString "0") (fromString "-") `shouldBe` Nothing
            merge (fromString "00") (fromString "01") `shouldBe` Just (fromString "0-")
            merge (fromString "001") (fromString "011") `shouldBe` Just (fromString "0-1")
            merge (fromString "001") (fromString "01-") `shouldBe` Nothing
            merge (fromString "-0-") (fromString "---") `shouldBe` Nothing

        it "works for the example value" $ do
            merge (fromString "000") (fromString "010") `shouldBe` Just (fromString "0-0")

    describe "flipNormalForm" $ do
        it "changes from CNF to DNF (and back)" $ do
            let minterms = map fromString ["010", "100", "111"]
            flipNormalForm minterms `shouldBe` map fromString ["111", "110", "100", "010", "001"]

        it "changes back to original (apart from the fact that the list is reversed) when applied twice" $ do
            let minterms = map fromString ["010", "100", "111"]
            (flipNormalForm . flipNormalForm) minterms `shouldBe` reverse minterms
            