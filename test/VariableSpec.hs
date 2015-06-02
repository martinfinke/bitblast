module VariableSpec where

import SpecHelper
import Variable
import Control.Monad(forM)

instance Arbitrary Variable where
    arbitrary = do
        i <- choose (0,10::Int)
        return $ eval initial $ var ('x' : show i)

randomVariables :: Int -> Gen [Variable]
randomVariables numvars = do
    let is = [0..numvars-1]
    forM is $ \i -> do
        return $ eval initial $ var ('x' : show i)


spec :: Spec
spec = do
    let (test1,test2,test3) = eval initial $ do
            t1 <- var "test1"
            t2 <- var "test2"
            t3 <- var "test3"
            return (t1,t2,t3)

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
