module QuineMcCluskeySpec where

import SpecHelper
import Control.Exception (evaluate)
import QuineMcCluskey
import TruthTable(var)
import Formula

spec :: Spec
spec = do
    describe "numRelevantLiterals" $ do
        it "is 0 for a Maxterm without negative literals" $ do
            numRelevantLiterals (Or [Atom (var 1), Atom (var 2)]) `shouldBe` 0

        it "is 2 for a Minterm with 2 positive literals" $ do
            numRelevantLiterals (And [Atom (var 2), Atom (var 1), Not $ Atom (var 1)]) `shouldBe` 2

        it "throws an error for a Formula that isn't a min/maxterm" $ do
            evaluate (numRelevantLiterals (Equiv [Atom (var 1)])) `shouldThrow` anyException