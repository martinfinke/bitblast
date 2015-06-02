module VariableSpec where

import SpecHelper
import Variable

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

    describe "Assignment get/setVarValue" $ do
        it "is Nothing for an empty assignment" $ do
            getVarValue test1 emptyAssignment `shouldBe` Nothing
        it "is Just True for an assignment where the variable was set to True" $ do
            getVarValue test1 (setVarValue test1 True emptyAssignment) `shouldBe` Just True
        it "is Nothing for an assignment where another variable was set" $ do
            getVarValue test2 (setVarValue test1 True emptyAssignment) `shouldBe` Nothing
        it "is equal for two assignments where the same variable was set to the same value" $ do
            let a1 = setVarValue test1 True emptyAssignment
            let a2 = setVarValue test1 True emptyAssignment
            a1 `shouldBe` a2
        it "is not equal for two assignments where the same variable was set to a different value" $ do
            let a1 = setVarValue test1 True emptyAssignment
            let a2 = setVarValue test1 False emptyAssignment
            (a1 == a2) `shouldBe` False
    describe "TruthTable get/setRow" $ do
        let assignment1 = setVarValue test1 True emptyAssignment
        let assignment2 = setVarValue test2 True emptyAssignment
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
