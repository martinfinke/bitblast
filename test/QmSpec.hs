module QmSpec where

import SpecHelper
import Qm
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
            let test cubes expected = compute_primes (Set.fromList cubes) `shouldBe` Set.fromList (map QmTerm expected)

            test [] []
            test [0] [(0, 0)]

            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]

            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]
            test [3] [(3, 0)]
            test [0,1] [(0, 1)]
            test [1,2] [(2, 0), (1, 0)]
            test [2,3] [(2, 1)]
            test [3,4] [(3, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4] [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] [(2, 4), (2, 1)]
            test [3,4,8] [(3, 0), (8, 0), (4, 0)]

            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]
            test [3] [(3, 0)]
            test [0,1] [(0, 1)]
            test [1,2] [(2, 0), (1, 0)]
            test [2,3] [(2, 1)]
            test [3,4] [(3, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4] [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] [(2, 4), (2, 1)]
            test [3,4,8] [(3, 0), (8, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4,9] [(2, 0), (1, 8), (4, 0)]
            test [2,3,6,13] [(2, 1), (2, 4), (13, 0)]
            test [3,4,8,12] [(3, 0), (8, 4), (4, 8)]


    describe "qm" $ do
        it "behaves as the python version" $ do
            let test ones zeros dc expected = Set.fromList (qm ones zeros dc) `shouldBe` Set.fromList (map fromString expected)
            test [1, 2, 5] [] [0, 7] ["-01", "0-0"]
            test [1, 2, 4, 5, 9, 13, 15, 16, 18] [] [0, 7] ["0--01", "-00-0", "0-1-1", "00-0-"]
            test [] [1] [] ["0"]
            test [] [2] [] ["0-", "-1"]
            test [] [2,3] [] ["0-"]
            test [] [2,3,4] [] ["00-", "11-", "1-1"]
            test [] [2,3,4,5] [7,13] ["-11-", "-00-", "1---"]
            test [1,6] [2,3,4,5] [] ["00-", "11-"]
            test [1,6,9,13] [2,3,4,5,11] [] ["-11-", "-00-", "11--"]

        it "doesn't simplify XOR (because it's impossible)" $ do
            let terms = map fromString ["01", "10"]
            qm (map getTerm terms) [] [] `shouldBe` terms

    describe "qm" $ do
        it "doesn't simplify XOR (because it's impossible)" $ do
            let terms = map fromString ["11", "00"]
            qm (map getTerm terms) [] [] `shouldBe` reverse terms

        it "simplifies a 1-variable CNF with redundancies" $ do
            let terms = map fromString ["0", "1"]
            qm (map getTerm terms) [] [] `shouldBe` map fromString ["-"]

        it "simplifies a 2-variable CNF with redundancies" $ do
            let terms = map fromString ["10", "11"]
            qm (map getTerm terms) [] [] `shouldBe` map fromString ["1-"]

        it "simplifies a 3-variable CNF with redundancies" $ do
            let terms = map fromString ["1001", "1011"]
            qm (map getTerm terms) [] [] `shouldBe` map fromString ["10-1"]


    describe "Example 1" $ do
        -- Youtube: pQ3MfzqGlrc
        let terms = Set.fromList $ map (getTerm . fromString) ["010", "100", "101", "110", "111"]

        it "finds the correct primes" $ do
            compute_primes terms `shouldBe` Set.fromList (map fromString ["-10", "1--"])

    describe "Example 2" $ do
        -- Youtube: pQ3MfzqGlrc
        let terms = Set.fromList $ map (getTerm . fromString) ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]

        it "finds the correct primes" $ do
            compute_primes terms `shouldBe` Set.fromList (map fromString ["1-10", "00--", "-111", "-0-0", "111-", "0--1"])

        it "finds the correct minimal subset of primes" $ do
            qm (Set.toAscList terms) [] [] `shouldBe` map fromString ["-0-0", "0--1", "111-"]

    describe "Example 3" $ do
        -- Youtube: bkH0T3fArUI
        let primes = map fromString ["0--0", "-1-0", "001-", "010-", "-011", "1-11", "111-"]
        let terms = map getTerm $ map fromString ["0010", "0101", "0110", "1011", "1100", "1110", "1111"]

        it "finds the correct minimum cover" $ do
            snd (unate_cover primes terms) `shouldBe` (Set.fromList $ map fromString ["-1-0", "001-", "010-", "1-11"])

    describe "2-Bit Multiplier" $ do
        -- http://research.ijcaonline.org/volume42/number4/pxc3877719.pdf
        let minterms = map fromString [
            --   a   *   b   =   c
                "00" ++ "00" ++ "0000",
                "00" ++ "01" ++ "0000",
                "00" ++ "10" ++ "0000",
                "00" ++ "11" ++ "0000",
                "01" ++ "00" ++ "0000",
                "01" ++ "01" ++ "0001",
                "01" ++ "10" ++ "0010",
                "01" ++ "11" ++ "0011",

                "10" ++ "00" ++ "0000",
                "10" ++ "01" ++ "0010",
                "10" ++ "10" ++ "0100",
                "10" ++ "11" ++ "0110",
                "11" ++ "00" ++ "0000",
                "11" ++ "01" ++ "0011",
                "11" ++ "10" ++ "0110",
                "11" ++ "11" ++ "1001"
                ]
        it "minimizes the DNF" $ do
            qm (map getTerm minterms) [] [] `shouldBe` map fromString [
                "00--0000",
                "--000000",
                "01010001",
                "01100010",
                "01110011",
                "10010010",
                "10100100",
                "10110110",
                "11010011",
                "11100110",
                "11111001"
                ]