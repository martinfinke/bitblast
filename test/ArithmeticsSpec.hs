module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import TruthTable
import Control.Monad(forM_)
import MinimizeFormula

spec :: Spec
spec = do
    let trueOnlyForAssignments assignments numVariables = setOutputs (zip assignments $ repeat (Just True)) (allFalseTable numVariables)
    describe "halfAdder" $ do
        it "has the correct truth table" $ do
            let [c,s,y,x] = map (Atom . var) [0..3]
            let ha = halfAdder (x,y) (s,c)
            let trueAssignments = map assignmentFromString [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            toTruthTable ha `shouldBe` trueOnlyForAssignments trueAssignments 4
            
    describe "fullAdder" $ do
        let [s,cOut,cIn,y,x] = map (Atom . var) [0..4]
        let fa = fullAdder (x,y) (cIn,cOut) s
        it "has the correct truth table" $ do
            let trueAssignments = map assignmentFromString [
                    "00000",
                    "00101",
                    "01001",
                    "01110",
                    "10001",
                    "10110",
                    "11010",
                    "11111"
                    ]
            toTruthTable fa `shouldBe` trueOnlyForAssignments trueAssignments 5