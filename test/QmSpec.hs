module QmSpec where

import SpecHelper
import Qm
import qualified Data.Set as Set
import qualified Data.ByteString as B

strToBS :: String -> B.ByteString
strToBS = B.pack . map read . map (:[])

spec :: Spec
spec = do
    describe "active_primes" $ do
        it "behaves as the python version" $ do
            active_primes 0 [] `shouldBe` []
            active_primes 1 [] `shouldBe` []
            active_primes 2 [] `shouldBe` []
            active_primes 0 [strToBS "0"] `shouldBe` []
            active_primes 0 [strToBS "1"] `shouldBe` []
            active_primes 1 [strToBS "1"] `shouldBe` [strToBS "1"]
            active_primes 2 [strToBS "1"] `shouldBe` []
            active_primes 0 [strToBS "11"] `shouldBe` []
            active_primes 1 [strToBS "11"] `shouldBe` [strToBS "11"]
            active_primes 2 [strToBS "11"] `shouldBe` []
            active_primes 3 [strToBS "11"] `shouldBe` [strToBS "11"]
            active_primes 4 [strToBS "11"] `shouldBe` []
            active_primes 0 [strToBS "01"] `shouldBe` []
            active_primes 1 [strToBS "01"] `shouldBe` [strToBS "01"]
            active_primes 2 [strToBS "01"] `shouldBe` []

            active_primes 0 (map strToBS ["0", "1"]) `shouldBe` []
            active_primes 1 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["1"])
            active_primes 2 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["0"])
            active_primes 3 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["0", "1"])
            active_primes 4 (map strToBS ["0", "1"]) `shouldBe` []
            active_primes 5 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["1"])
            active_primes 6 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["0"])
            active_primes 7 (map strToBS ["0", "1"]) `shouldBe` (map strToBS ["0", "1"])

            active_primes 0 (map strToBS ["10", "11"]) `shouldBe` []
            active_primes 1 (map strToBS ["10", "11"]) `shouldBe` (map strToBS ["11"])
            active_primes 2 (map strToBS ["10", "11"]) `shouldBe` (map strToBS ["10"])
            active_primes 3 (map strToBS ["10", "11"]) `shouldBe` (map strToBS ["10", "11"])
            active_primes 4 (map strToBS ["10", "11"]) `shouldBe` []

    describe "is_full_cover" $ do
        it "behaves as the python version" $ do
            let cover primes ones = is_full_cover (map strToBS primes) (Set.fromList $ map strToBS ones)
            cover [] [] `shouldBe` True
            cover [] ["0"] `shouldBe` False
            cover ["0"] ["0"] `shouldBe` True
            cover ["0", "1"] ["0"] `shouldBe` True
            cover ["0"] ["0", "1"] `shouldBe` False
            cover ["0", "1"] ["0", "1"] `shouldBe` True
            cover ["2"] ["0", "1"] `shouldBe` True
            cover ["021"] ["0", "1"] `shouldBe` False
            cover ["021"] ["011", "101"] `shouldBe` False
            cover ["021", "201"] ["011", "101"] `shouldBe` True

    describe "is_cover" $ do
        it "behaves as the python version" $ do
            is_cover (strToBS "") (strToBS "") `shouldBe` True
            is_cover (strToBS "") (strToBS "0") `shouldBe` True
            is_cover (strToBS "") (strToBS "1") `shouldBe` True
            is_cover (strToBS "0") (strToBS "") `shouldBe` True
            is_cover (strToBS "1") (strToBS "") `shouldBe` True
            is_cover (strToBS "0") (strToBS "0") `shouldBe` True
            is_cover (strToBS "0") (strToBS "1") `shouldBe` False
            is_cover (strToBS "1") (strToBS "0") `shouldBe` False
            is_cover (strToBS "2") (strToBS "0") `shouldBe` True
            is_cover (strToBS "2") (strToBS "1") `shouldBe` True
            is_cover (strToBS "2") (strToBS "2") `shouldBe` True
            is_cover (strToBS "021") (strToBS "011") `shouldBe` True
            is_cover (strToBS "021") (strToBS "001") `shouldBe` True
            is_cover (strToBS "021") (strToBS "000") `shouldBe` False
            is_cover (strToBS "021") (strToBS "101") `shouldBe` False

    describe "compute_primes" $ do
        it "behaves as the python version" $ do
            let test cubes vars expected = compute_primes (Set.fromList $ map strToBS cubes) vars `shouldBe` (Set.fromList $ map strToBS expected)
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
            test ["01", "12"] 0 []
            test ["01", "12"] 1 ["01", "12"]
            test ["01", "12"] 2 ["01", "12"]
            test ["01", "12"] 3 ["01", "12"]


    describe "bitcount" $ do
        it "behaves as the python version" $ do
            bitcount (strToBS "") `shouldBe` 0
            bitcount (strToBS "0") `shouldBe` 0
            bitcount (strToBS "1") `shouldBe` 1
            bitcount (strToBS "10") `shouldBe` 1
            bitcount (strToBS "010") `shouldBe` 1
            bitcount (strToBS "0110") `shouldBe` 2

    describe "b2s" $ do
        it "behaves as the python version" $ do
            b2s 0 0 `shouldBe` strToBS ""
            b2s 0 1 `shouldBe` strToBS "0"
            b2s 0 2 `shouldBe` strToBS "00"
            b2s 0 3 `shouldBe` strToBS "000"
            b2s 0 4 `shouldBe` strToBS "0000"

            b2s 1 0 `shouldBe` strToBS ""
            b2s 1 1 `shouldBe` strToBS "1"
            b2s 1 2 `shouldBe` strToBS "01"
            b2s 1 3 `shouldBe` strToBS "001"
            b2s 1 4 `shouldBe` strToBS "0001"

            b2s 2 0 `shouldBe` strToBS ""
            b2s 2 1 `shouldBe` strToBS "0"
            b2s 2 2 `shouldBe` strToBS "10"
            b2s 2 3 `shouldBe` strToBS "010"
            b2s 2 4 `shouldBe` strToBS "0010"

            b2s 3 0 `shouldBe` strToBS ""
            b2s 3 1 `shouldBe` strToBS "1"
            b2s 3 2 `shouldBe` strToBS "11"
            b2s 3 3 `shouldBe` strToBS "011"
            b2s 3 4 `shouldBe` strToBS "0011"

    describe "merge" $ do
        it "behaves as the python version" $ do
            merge (strToBS "") (strToBS "") `shouldBe` Just (strToBS "")
            merge (strToBS "") (strToBS "1") `shouldBe` Just (strToBS "")
            merge (strToBS "") (strToBS "11") `shouldBe` Just (strToBS "")
            merge (strToBS "1") (strToBS "") `shouldBe` Just (strToBS "")
            merge (strToBS "11") (strToBS "") `shouldBe` Just (strToBS "")

            merge (strToBS "0") (strToBS "0") `shouldBe` Just (strToBS "0")
            merge (strToBS "0") (strToBS "1") `shouldBe` Just (strToBS "2")
            merge (strToBS "0") (strToBS "2") `shouldBe` Nothing
            merge (strToBS "00") (strToBS "01") `shouldBe` Just (strToBS "02")
            merge (strToBS "001") (strToBS "011") `shouldBe` Just (strToBS "021")
            merge (strToBS "001") (strToBS "012") `shouldBe` Nothing
            merge (strToBS "202") (strToBS "222") `shouldBe` Nothing
