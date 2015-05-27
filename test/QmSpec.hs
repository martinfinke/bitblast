module QmSpec where

import SpecHelper
import Qm
import QmTerm(fromString, flipNormalForm)
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
            let test cubes vars expected = compute_primes False (Set.fromList $ map fromString cubes) vars `shouldBe` Set.fromList (map fromString expected)
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
            qm (map s2b terms) [] [] `shouldBe` terms

    describe "qmCnf" $ do
        it "doesn't simplify XOR (because it's impossible)" $ do
            let terms = map fromString ["11", "00"]
            qmCnf (map s2b terms) [] [] `shouldBe` reverse terms

        it "simplifies a 1-variable CNF with redundancies" $ do
            let terms = map fromString ["0", "1"]
            qmCnf (map s2b terms) [] [] `shouldBe` map fromString ["-"]

        it "simplifies a 2-variable CNF with redundancies" $ do
            let terms = map fromString ["10", "11"]
            qmCnf (map s2b terms) [] [] `shouldBe` map fromString ["1-"]

        it "simplifies a 3-variable CNF with redundancies" $ do
            let terms = map fromString ["1001", "1011"]
            qmCnf (map s2b terms) [] [] `shouldBe` map fromString ["10-1"]

    describe "Example 1" $ do
        -- Youtube: pQ3MfzqGlrc
        let numVars = 3
        let terms = Set.fromList $ map fromString ["010", "100", "101", "110", "111"]

        it "finds the correct primes" $ do
            compute_primes False terms numVars `shouldBe` Set.fromList (map fromString ["-10", "1--"])

    describe "Example 2" $ do
        -- Youtube: pQ3MfzqGlrc
        let numVars = 4
        let terms = Set.fromList $ map fromString ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]

        it "finds the correct primes" $ do
            compute_primes False terms numVars `shouldBe` Set.fromList (map fromString ["1-10", "00--", "-111", "-0-0", "111-", "0--1"])

        it "finds the correct minimal subset of primes" $ do
            qm (map s2b $ Set.toList terms) [] [] `shouldBe` map fromString ["-0-0", "0--1", "111-"]

    describe "Example 3" $ do
        -- Youtube: bkH0T3fArUI
        let primes = Set.fromList $ map fromString ["0--0", "-1-0", "001-", "010-", "-011", "1-11", "111-"]
        let terms = Set.fromList $ map fromString ["0010", "0101", "0110", "1011", "1100", "1110", "1111"]

        it "finds the correct minimum cover" $ do
            unate_cover primes terms `shouldBe` map fromString ["-1-0", "001-", "010-", "1-11"]

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
            qm (map s2b minterms) [] [] `shouldBe` map fromString [
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

        