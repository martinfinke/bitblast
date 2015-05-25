module QmSpec where

import SpecHelper
import Qm
import qualified Data.Set as Set
import qualified Data.ByteString as B

fromString :: String -> B.ByteString
fromString = B.pack . map read . map (:[])

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
            cover ["2"] ["0", "1"] `shouldBe` True
            cover ["021"] ["0", "1"] `shouldBe` False
            cover ["021"] ["011", "101"] `shouldBe` False
            cover ["021", "201"] ["011", "101"] `shouldBe` True

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
            is_cover (fromString "2") (fromString "0") `shouldBe` True
            is_cover (fromString "2") (fromString "1") `shouldBe` True
            is_cover (fromString "2") (fromString "2") `shouldBe` True
            is_cover (fromString "021") (fromString "011") `shouldBe` True
            is_cover (fromString "021") (fromString "001") `shouldBe` True
            is_cover (fromString "021") (fromString "000") `shouldBe` False
            is_cover (fromString "021") (fromString "101") `shouldBe` False

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
            test ["01", "12"] 0 []
            test ["01", "12"] 1 ["01", "12"]
            test ["01", "12"] 2 ["01", "12"]
            test ["01", "12"] 3 ["01", "12"]


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

    describe "merge" $ do
        it "behaves as the python version" $ do
            merge (fromString "") (fromString "") `shouldBe` Just (fromString "")
            merge (fromString "") (fromString "1") `shouldBe` Just (fromString "")
            merge (fromString "") (fromString "11") `shouldBe` Just (fromString "")
            merge (fromString "1") (fromString "") `shouldBe` Just (fromString "")
            merge (fromString "11") (fromString "") `shouldBe` Just (fromString "")

            merge (fromString "0") (fromString "0") `shouldBe` Just (fromString "0")
            merge (fromString "0") (fromString "1") `shouldBe` Just (fromString "2")
            merge (fromString "0") (fromString "2") `shouldBe` Nothing
            merge (fromString "00") (fromString "01") `shouldBe` Just (fromString "02")
            merge (fromString "001") (fromString "011") `shouldBe` Just (fromString "021")
            merge (fromString "001") (fromString "012") `shouldBe` Nothing
            merge (fromString "202") (fromString "222") `shouldBe` Nothing

        it "works for the example value" $ do
            merge (fromString "000") (fromString "010") `shouldBe` Just (fromString "020")

    describe "qm" $ do
        it "behaves as the python version" $ do
            let test ones zeros dc expected = Set.fromList (qm ones zeros dc) `shouldBe` Set.fromList (map fromString expected)
            test [1, 2, 5] [] [0, 7] ["201", "020"]
            test [1, 2, 4, 5, 9, 13, 15, 16, 18] [] [0, 7] ["02201", "20020", "02121", "00202"]
            test [] [1] [] ["0"]
            test [1] [2] [] ["21"]
            test [1] [2,3,4,5] [] ["002"]
            test [1,6,9,13] [2,3,4,5] [] ["2112", "2002", "1222"]
            test [] [2,3,4,5] [7,13] ["2112", "2002", "1222"]