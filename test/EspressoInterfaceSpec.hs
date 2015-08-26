module EspressoInterfaceSpec where

import SpecHelper
import EspressoInterface
import TruthBasedCore(CNF(..), Assignment, Clause(..), lit, assignments)

spec :: Spec
spec = do
    describe "espressoOptimize" $ do
        it "minimizes a formula with one variable" $ do
            let zeros = [[True, True]]
            result <- espressoOptimize 2 zeros
            result `shouldBe` CNF [Clause [lit 1 False, lit 2 False]]
        it "minimizes a tautology" $ do
            let zeros = []
            --result <- espressoOptimize 1 zeros
            pending
            --result `shouldBe` CNF []
