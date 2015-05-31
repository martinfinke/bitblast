module SatchmoInterfaceSpec where

import SpecHelper
import SatchmoInterface
import Qm

import qualified Data.Map as Map

spec :: Spec
spec = do
    let term = getTerm . fromString
    describe "columns" $ do
        it "is the empty map for no primes and no ones" $ do
            columns [] [] `shouldBe` Map.fromList []
        it "is one mapping for a minterm that's covered" $ do
            let prime = fromString "-"
            let minterm = term "1"
            columns [prime] [minterm] `shouldBe` Map.fromList [(minterm, [0])]
        it "does the right thing for multiple terms and primes" $ do
            let primes = map fromString [
                    "1-10", -- 0
                    "00--", -- 1
                    "-111", -- 2
                    "-0-0", -- 3
                    "111-", -- 4
                    "0--1"  -- 5
                    ]
            let minterms = map term ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]
            columns primes minterms `shouldBe` Map.fromList [
                    (term "0000", [1,3]),
                    (term "0001", [1,5]),
                    (term "0010", [1,3]),
                    (term "1000", [3]),
                    (term "0011", [1,5]),
                    (term "0101", [5]),
                    (term "1010", [0,3]),
                    (term "0111", [2,5]),
                    (term "1110", [0,4]),
                    (term "1111", [2,4])
                    ]

        it "throws an error if a minterm isn't covered" $ do
            let prime = fromString "-1"
            let minterm = term "10"
            evaluate (columns [prime] [minterm]) `shouldThrow` anyException