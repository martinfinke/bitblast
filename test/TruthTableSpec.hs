module TruthTableSpec where

import SpecHelper
import TruthTable
import Control.Exception (evaluate)


spec :: Spec
spec = do
    describe "Creating a Variable" $ do
        it "allows creating a variable with index zero" $ do
            let indexZeroVar = var 0
            indexZeroVar `shouldBe` indexZeroVar

        it "allows creating a variable with the maximum possible index" $ do
            let maxIndexVar = var (fromEnum (maxBound::Variable))
            maxIndexVar `shouldBe` maxIndexVar

        it "allows creating a variable inside the allowed range" $ do
            let minIndex = fromEnum (minBound::Variable)
            let maxIndex = fromEnum (maxBound::Variable)
            let inBetweenIndex = (minIndex+maxIndex) `div` 2
            let inBetweenVar = var inBetweenIndex
            inBetweenVar `shouldBe` inBetweenVar

        it "doesn't allow variables with negative index" $ do
            evaluate (var (-1)) `shouldThrow` anyErrorCall

        it "doesn't allow variables with an index that is too high" $ do
            evaluate (var $ fromEnum (maxBound::Variable) + 1) `shouldThrow` anyErrorCall


    describe "Assignment" $ do
        it "sets and gets any Variable value correctly" $ do
            property $ \assignment variable bool -> getVariable variable (setVariable variable bool assignment) == bool
            
    describe "TruthTable emptyTable" $ do
        it "can create an empty table with any valid size" $ do
            property $ \(TenOrLess numVariables) -> numVariablesInTable (emptyTable numVariables) == numVariables

        it "doesn't allow creating a table that's too large" $ do
            evaluate (emptyTable $ maxNumVariables+1) `shouldThrow` anyErrorCall

    describe "TruthTable setOutput" $ do
        it "doesn't allow setting an output value that's out of bounds" $ do
            let table = emptyTable 3
            evaluate (setOutput (toEnum 8) F table) `shouldThrow` anyErrorCall

        it "sets any valid assignment correctly" $ do
            let randomAssignment table i = toEnum $ max 0 $ min (i-1) (numVariablesInTable table - 1)
            property $ \table (TenOrLess i) outputValue ->
                getOutput (randomAssignment table i) (setOutput (randomAssignment table i) outputValue table) == outputValue

    describe "TruthTable setOutputs" $ do
        it "sets multiple outputs correctly" $ do
            let outputValues = [
                    (allFalse, T)
                    , (setVariable (var 0) True allFalse, F)
                    ]
            let empty = emptyTable 3
            let t1 = setOutputs outputValues empty
            getOutput allFalse t1 `shouldBe` T
            getOutput (setVariable (var 0) True allFalse) t1 `shouldBe` F
