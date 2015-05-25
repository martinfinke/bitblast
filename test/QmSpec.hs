module QmSpec where

import SpecHelper
import Qm
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = do
    describe "active_primes" $ do
        it "behaves as the python version" $ do
            active_primes 0 [] `shouldBe` []
            active_primes 1 [] `shouldBe` []
            active_primes 2 [] `shouldBe` []
            active_primes 0 [fromString "0"] `shouldBe` []
            active_primes 0 [fromString "1"] `shouldBe` []
            active_primes 1 [fromString "1"] `shouldBe` [fromString "1"]
            active_primes 2 [fromString "1"] `shouldBe` []
            active_primes 0 [fromString "11"] `shouldBe` []
            active_primes 1 [fromString "11"] `shouldBe` [fromString "11"]
            active_primes 2 [fromString "11"] `shouldBe` []
            active_primes 3 [fromString "11"] `shouldBe` [fromString "11"]
            active_primes 4 [fromString "11"] `shouldBe` []
            active_primes 0 [fromString "01"] `shouldBe` []
            active_primes 1 [fromString "01"] `shouldBe` [fromString "01"]
            active_primes 2 [fromString "01"] `shouldBe` []

            active_primes 0 (map fromString ["0", "1"]) `shouldBe` []
            active_primes 1 (map fromString ["0", "1"]) `shouldBe` (map fromString ["1"])
            active_primes 2 (map fromString ["0", "1"]) `shouldBe` (map fromString ["0"])
            active_primes 3 (map fromString ["0", "1"]) `shouldBe` (map fromString ["0", "1"])
            active_primes 4 (map fromString ["0", "1"]) `shouldBe` []
            active_primes 5 (map fromString ["0", "1"]) `shouldBe` (map fromString ["1"])
            active_primes 6 (map fromString ["0", "1"]) `shouldBe` (map fromString ["0"])
            active_primes 7 (map fromString ["0", "1"]) `shouldBe` (map fromString ["0", "1"])

            active_primes 0 (map fromString ["10", "11"]) `shouldBe` []
            active_primes 1 (map fromString ["10", "11"]) `shouldBe` (map fromString ["11"])
            active_primes 2 (map fromString ["10", "11"]) `shouldBe` (map fromString ["10"])
            active_primes 3 (map fromString ["10", "11"]) `shouldBe` (map fromString ["10", "11"])
            active_primes 4 (map fromString ["10", "11"]) `shouldBe` []

    describe "is_full_cover" $ do
        it "behaves as the python version" $ do
            let cover primes ones = is_full_cover (map fromString primes) (Set.fromList $ map fromString ones)
            cover [] [] `shouldBe` True
            cover [] ["0"] `shouldBe` False
            cover ["0"] ["0"] `shouldBe` True
            cover ["0", "1"] ["0"] `shouldBe` True
            cover ["0"] ["0", "1"] `shouldBe` False
            cover ["0", "1"] ["0", "1"] `shouldBe` True
            cover ["-"] ["0", "1"] `shouldBe` True
            cover ["0-1"] ["0", "1"] `shouldBe` False
            cover ["0-1"] ["011", "101"] `shouldBe` False
            cover ["0-1", "-01"] ["011", "101"] `shouldBe` True

    describe "is_cover" $ do
        it "behaves as the python version" $ do
            is_cover (fromString "") (fromString "") `shouldBe` True
            is_cover (fromString "") (fromString "0") `shouldBe` True
            is_cover (fromString "") (fromString "1") `shouldBe` True
            is_cover (fromString "0") (fromString "") `shouldBe` True
            is_cover (fromString "1") (fromString "") `shouldBe` True
            is_cover (fromString "0") (fromString "0") `shouldBe` True
            is_cover (fromString "0") (fromString "1") `shouldBe` False
            is_cover (fromString "1") (fromString "0") `shouldBe` False
            is_cover (fromString "-") (fromString "0") `shouldBe` True
            is_cover (fromString "-") (fromString "1") `shouldBe` True
            is_cover (fromString "-") (fromString "-") `shouldBe` True
            is_cover (fromString "0-1") (fromString "011") `shouldBe` True
            is_cover (fromString "0-1") (fromString "001") `shouldBe` True
            is_cover (fromString "0-1") (fromString "000") `shouldBe` False
            is_cover (fromString "0-1") (fromString "101") `shouldBe` False

    describe "compute_primes" $ do
        it "behaves as the python version" $ do
            let test cubes vars expected = compute_primes (Set.fromList $ map fromString cubes) vars `shouldBe` (Set.fromList $ map fromString expected)
            test [] 0 []
            test ["0"] 0 ["0"]
            test ["0"] 1 ["0"]
            test ["0"] 2 ["0"]
            test ["1"] 0 []
            test ["1"] 1 ["1"]
            test ["1"] 2 ["1"]
            test ["01"] 0 []
            test ["01"] 1 ["01"]
            test ["01"] 2 ["01"]
            test ["010"] 0 []
            test ["010"] 1 ["010"]
            test ["010"] 2 ["010"]
            test ["01", "10"] 0 []
            test ["01", "10"] 1 ["01", "10"]
            test ["01", "10"] 2 ["01", "10"]
            test ["01", "1-"] 0 []
            test ["01", "1-"] 1 ["01", "1-"]
            test ["01", "1-"] 2 ["01", "1-"]
            test ["01", "1-"] 3 ["01", "1-"]


    describe "bitcount" $ do
        it "behaves as the python version" $ do
            bitcount (fromString "") `shouldBe` 0
            bitcount (fromString "0") `shouldBe` 0
            bitcount (fromString "1") `shouldBe` 1
            bitcount (fromString "10") `shouldBe` 1
            bitcount (fromString "010") `shouldBe` 1
            bitcount (fromString "0110") `shouldBe` 2

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

    describe "Example 1" $ do
        -- Youtube: pQ3MfzqGlrc
        let numVars = 3
        let terms = Set.fromList $ map fromString ["010", "100", "101", "110", "111"]

        it "finds the correct primes" $ do
            compute_primes terms numVars `shouldBe` Set.fromList (map fromString ["-10", "1--"])

    describe "Example 2" $ do
        -- Youtube: pQ3MfzqGlrc
        let numVars = 4
        let terms = Set.fromList $ map fromString ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]

        it "finds the correct primes" $ do
            compute_primes terms numVars `shouldBe` Set.fromList (map fromString ["1-10", "00--", "-111", "-0-0", "111-", "0--1"])

        it "finds the correct minimal subset of primes" $ do
            qm [0,1,2,3,5,7,8,10,14,15] [] [] `shouldBe` map fromString ["-0-0", "0--1", "111-"]

    describe "Example 3" $ do
        -- Youtube: bkH0T3fArUI
        let primes = Set.fromList $ map fromString ["0--0", "-1-0", "001-", "010-", "-011", "1-11", "111-"]
        let terms = Set.fromList $ map fromString ["0010", "0101", "0110", "1011", "1100", "1110", "1111"]

        it "finds the correct minimum cover" $ do
            unate_cover primes terms `shouldBe` map fromString ["-1-0", "001-", "010-", "1-11"]

    describe "2-Bit Multiplier" $ do
        -- http://research.ijcaonline.org/volume42/number4/pxc3877719.pdf
        let minterms = map s2b $ map fromString [
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
            qm minterms [] [] `shouldBe` map fromString [
                "--000000",
                "00--0000",
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
