module EspressoInterfaceSpec where

import SpecHelper
import EspressoInterface
import TruthBasedCore(CNF(..), Assignment, Clause(..), lit, assignments)

spec :: Spec
spec = do
    describe "espressoOptimize" $ do
        it "minimizes a formula" $ do
            let zeros = [[True, True]]
            result <- espressoOptimize 2 zeros
            result `shouldBe` CNF [Clause [lit 1 False, lit 2 False]]
        it "minimizes a tautology" $ do
            result <- espressoOptimize 1 []
            result `shouldBe` CNF []
        it "minimizes a formula that is always False" $ do
            result <- espressoOptimize 1 [[True], [False]]
            result `shouldBe` CNF [Clause []]
        it "minimizes a tautology without variables" $ do
            result <- espressoOptimize 0 []
            result `shouldBe` CNF []
        it "minimizes a formula that is always false, without variables" $ do
            result <- espressoOptimize 0 [[]]
            result `shouldBe` CNF [Clause []]

