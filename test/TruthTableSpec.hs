module TruthTableSpec where

import SpecHelper
import TruthTable
import Control.Exception (evaluate)


instance Arbitrary Variable where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Assignment where
    arbitrary = resize (fromEnum (maxBound::Variable)) $ do
        trueVariables <- listOf1 arbitrary :: Gen [Variable]
        return $ setVariables allFalse (zip trueVariables (repeat True))

instance Arbitrary OutputValue where
    arbitrary = elements [T,F,DC]

instance Arbitrary TruthTable where
    arbitrary = resize 20 $ do
        (TenOrLess tenOrLess) <- arbitrary
        let numVariables = max 2 tenOrLess
        let empty = emptyTable numVariables
        let boundedAssignmentsGen = elements [minBound..toEnum (numVariables-1)] :: Gen Assignment
        randomAssignments <- listOf1 boundedAssignmentsGen
        randomOutputs <- vector (length randomAssignments) :: Gen [OutputValue]
        let rows = zip randomAssignments randomOutputs
        return $ setOutputs empty rows

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
        it "sets and gets a Variable value correctly" $ do
            property $ \assignment variable bool -> getVariable (setVariable assignment variable bool) variable == bool
            
    describe "TruthTable emptyTable" $ do
        it "can create an empty table with a valid size" $ do
            property $ \(TenOrLess numVariables) -> numVariablesInTable (emptyTable numVariables) == numVariables

        it "doesn't allow creating a table that's too large" $ do
            evaluate (emptyTable $ maxNumVariables+1) `shouldThrow` anyErrorCall

    describe "TruthTable setOutput" $ do
        it "doesn't allow setting an output value that's out of bounds" $ do
            let table = emptyTable 3
            evaluate (setOutput table (toEnum 8) F) `shouldThrow` anyErrorCall

        it "sets a valid assignment correctly" $ do
            let randomAssignment table i = toEnum $ max 0 $ min (i-1) (numVariablesInTable table - 1)
            property $ \table (TenOrLess i) outputValue ->
                getOutput (setOutput table (randomAssignment table i) outputValue) (randomAssignment table i) == outputValue

    describe "TruthTable setOutputs" $ do
        it "sets multiple outputs correctly" $ do
            let outputValues = [
                    (allFalse, T)
                    , (setVariable allFalse (var 0) True, F)
                    ]
            let empty = emptyTable 3
            let t1 = setOutputs empty outputValues
            getOutput t1 allFalse `shouldBe` T
            getOutput t1 (setVariable allFalse (var 0) True) `shouldBe` F

    describe "toInternal" $ do
        it "is inverse to fromInternal" $ do
            property $ \outputValue -> (fromInternal . toInternal) outputValue == outputValue