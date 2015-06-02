module VariableSpec where

import SpecHelper
import Variable

spec :: Spec
spec = do
    describe "var" $ do
        it "can create two variables and keep them separate" $ do
            let (x',y') = eval initial $ do
                    x <- var "x"
                    y <- var "y"
                    return (x,y)
            (x' == y') `shouldBe` False