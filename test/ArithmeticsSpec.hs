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
            let [c,s,y,x] = map var [0..3]
            let ha = halfAdder (x,y) (s,c)
            let shouldBeTrue = map assignmentFromString [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            let expectedTruthTable = setOutputs (zip shouldBeTrue $ repeat (Just True)) (allFalseTable 4)
            toTruthTable ha `shouldBe` expectedTruthTable
            
    describe "fullAdder" $ do
        it "has the correct truth table" $ do
            let [s,cOut,cIn,y,x] = map var [0..4]
            let fa = fullAdder (x,y) (cIn,cOut) s
            let shouldBeTrue = map assignmentFromString [
                    "00000",
                    "00101",
                    "01001",
                    "01110",
                    "10001",
                    "10110",
                    "11010",
                    "11111"
                    ]
            let expectedTruthTable = setOutputs (zip shouldBeTrue $ repeat (Just True)) (allFalseTable 5)
            toTruthTable fa `shouldBe` expectedTruthTable

            