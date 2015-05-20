module QuineMcCluskeySpec where

import SpecHelper
import Control.Exception (evaluate)
import QuineMcCluskey
import TruthTable(var)
import Formula
import qualified Data.Map.Lazy as Map

spec :: Spec
spec = do
    describe "numRelevantLiterals" $ do
        it "is 0 for a Maxterm without negative literals" $ do
            numRelevantLiterals (Or [Atom (var 1), Atom (var 2)]) `shouldBe` 0

        it "is 2 for a Minterm with 2 positive literals" $ do
            numRelevantLiterals (And [Atom (var 2), Atom (var 1), Not $ Atom (var 1)]) `shouldBe` 2

        it "throws an error for a Formula that isn't a min/maxterm" $ do
            evaluate (numRelevantLiterals (Equiv [Atom (var 1)])) `shouldThrow` anyException

    describe "groupByRelevantLiterals" $ do
        it "groups a CNF by number of negative literals" $ do
            let cnf = And [
                    Or [Atom (var 0), Not $ Atom (var 1)],
                    Or [Not $ Atom (var 0), Not $ Atom (var 1)],
                    Or [Not $ Atom (var 0), Atom (var 1)]
                    ]
            groupByRelevantLiterals cnf `shouldBe` Map.fromList [
                (1, [Or [Atom (var 0), Not $ Atom (var 1)], Or [Not $ Atom (var 0), Atom (var 1)]]),
                (2, [Or [Not $ Atom (var 0), Not $ Atom (var 1)]])
                ]

        it "groups a DNF by number of negative literals" $ do
            let dnf = Or [
                    And [Atom (var 0), Not $ Atom (var 1)],
                    And [Not $ Atom (var 0), Not $ Atom (var 1)],
                    And [Not $ Atom (var 0), Atom (var 1)]
                    ]
            groupByRelevantLiterals dnf `shouldBe` Map.fromList [
                (1, [And [Atom (var 0), Not $ Atom (var 1)], And [Not $ Atom (var 0), Atom (var 1)]]),
                (0, [And [Not $ Atom (var 0), Not $ Atom (var 1)]])
                ]