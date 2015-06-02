module VariableSpec where

import SpecHelper
import Variable
import Control.Monad(forM)
import qualified Data.Set as Set

instance Arbitrary Variable where
    arbitrary = do
        candidates <- randomVariables 10
        elements candidates

randomVariables :: Int -> Gen [Variable]
randomVariables numvars = do
    let vars = eval initial $ do
            forM [0..numvars-1] $ const var
    return vars

instance Arbitrary Assignment where
    arbitrary = do
        numvars <- choose (1,10::Int)
        variables <- randomVariables numvars
        bools <- vectorOf numvars (arbitrary::Gen Bool)
        return $ assignmentFromList (zip variables bools)

spec :: Spec
spec = do
    let ([test1,test2,test3],posMapping) = generateVars 3

    describe "var" $ do
        it "can create variables and keep them separate" $ do
            (test1 == test2) `shouldBe` False
            (test1 == test3) `shouldBe` False
            (test2 == test3) `shouldBe` False

    describe "Assignment get/setVar" $ do
        it "is Nothing for an empty assignment" $ do
            getVar test1 emptyAssignment `shouldBe` Nothing
        it "is Just True for an assignment where the variable was set to True" $ do
            getVar test1 (setVar test1 True emptyAssignment) `shouldBe` Just True
        it "is Nothing for an assignment where another variable was set" $ do
            getVar test2 (setVar test1 True emptyAssignment) `shouldBe` Nothing
        it "is equal for two assignments where the same variable was set to the same value" $ do
            let a1 = setVar test1 True emptyAssignment
            let a2 = setVar test1 True emptyAssignment
            a1 `shouldBe` a2
        it "is not equal for two assignments where the same variable was set to a different value" $ do
            let a1 = setVar test1 True emptyAssignment
            let a2 = setVar test1 False emptyAssignment
            (a1 == a2) `shouldBe` False

    describe "assignmentFromList" $ do
        it "is the empty assignment for an empty list" $ do
            assignmentFromList [] `shouldBe` emptyAssignment
        it "is the same as setting variables using setVar" $ do
            assignmentFromList [(test1, True)] `shouldBe` setVar test1 True emptyAssignment
        it "doesn't set variables that aren't in the list" $ do
            let a = assignmentFromList [(test2, False)]
            getVar test1 a `shouldBe` Nothing

    describe "assignmentFromString" $ do
        it "is the emptyAssignment for an empty string" $ do
            assignmentFromString [] "" `shouldBe` emptyAssignment
        it "throws an error if the positionMapping doesn't match the string" $ do
            evaluate (assignmentFromString [test1] "01") `shouldThrow` anyException
        it "works for one variable" $ do
            getVar test2 (assignmentFromString [test2] "1") `shouldBe` Just True
        it "doesn't assign other variables" $ do
            getVar test2 (assignmentFromString [test1] "1") `shouldBe` Nothing
        it "expects the last-added variable on the left end" $ do
            let assignment = (assignmentFromString [test2,test1] "10")
            getVar test2 assignment `shouldBe` Just True
            getVar test1 assignment `shouldBe` Just False

    describe "assignmentToString" $ do
        it "is the empty string for an empty assignment" $ do
            assignmentToString [] emptyAssignment `shouldBe` ""
        it "creates a dash if a variable isn't in the assignment" $ do
            assignmentToString [test1] emptyAssignment `shouldBe` "-"
            assignmentToString posMapping emptyAssignment `shouldBe` "---"
        it "creates a one for a true value" $ do
            assignmentToString [test1] (setVar test1 True emptyAssignment) `shouldBe` "1"
        it "creates a zero for a false value" $ do
            assignmentToString posMapping (setVar test1 False emptyAssignment) `shouldBe` "--0"

    describe "expandOrReduce" $ do
        it "does nothing if the variableSet is the same as in the assignment" $ do
            let assignment = setVar test1 True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [test1]) assignment
            assignment' `shouldBe` assignment
        it "reduces to an emptyAssignment if the variableSet is empty" $ do
            let assignment = setVar test1 True emptyAssignment
            let shouldBeEmpty = expandOrReduce True Set.empty assignment
            shouldBeEmpty `shouldBe` emptyAssignment
        it "removes a variable that's not in the variable set" $ do
            let assignment = setVar test1 True $ setVar test2 False emptyAssignment
            let assignment' = expandOrReduce True (Set.fromList [test2]) assignment
            assignment' `shouldBe` setVar test2 False emptyAssignment
        it "assigns a variable that's not yet in the assignment" $ do
            let assignment = setVar test1 True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [test1, test2]) assignment
            let expected = setVar test1 True $ setVar test2 False emptyAssignment
            assignment' `shouldBe` expected
        it "can add and remove at the same time" $ do
            let assignment = setVar test1 False $ setVar test2 True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [test2, test3]) assignment
            let expected = setVar test3 False $ setVar test2 True emptyAssignment
            assignment' `shouldBe` expected
            
    describe "TruthTable get/setRow" $ do
        let assignment1 = setVar test1 True emptyAssignment
        let assignment2 = setVar test2 True emptyAssignment
        let table1 = setRow assignment1 True emptyTable
        let table2 = setRow assignment2 True emptyTable
        it "is Nothing for an empty table" $ do
            getRow emptyAssignment emptyTable `shouldBe` Nothing
        it "is Just True for an assignment that has been set" $ do
            getRow emptyAssignment (setRow emptyAssignment True emptyTable) `shouldBe` Just True
        it "is Nothing if a different assignment has been set" $ do
            getRow assignment1 (setRow assignment2 True emptyTable) `shouldBe` Nothing
        it "creates two different tables if the rows being modified are different" $ do
            (table1 == table2) `shouldBe` False
