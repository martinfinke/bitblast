module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import TruthTable
import Control.Monad(forM_)

spec :: Spec
spec = do
    describe "halfAdder" $ do
        it "has the correct truth table" $ do
            let [x,y,s,c] = map var [3,2,1,0]
            let ha = halfAdder (x,y) (s,c)
            let shouldBeTrue = map assignmentFromString [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            let shouldBeFalse = map assignmentFromString [
                    "0010",
                    "0001",
                    "0011",
                    "0100",
                    "0101",
                    "0111",
                    "1000",
                    "1001",
                    "1011",
                    "1100",
                    "1110",
                    "1111"
                    ]
            forM_ shouldBeTrue $ \assignment -> eval assignment ha `shouldBe` True
            forM_ shouldBeFalse $ \assignment -> eval assignment ha `shouldBe` False
            