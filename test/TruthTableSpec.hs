module TruthTableSpec where

import SpecHelper
import Assignment
import TruthTable
import Formula
import Variable(var)
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "TruthTable get/setRow" $ do
        let assignment1 = setVar (var 0) True emptyAssignment
        let assignment2 = setVar (var 1) True emptyAssignment
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

    describe "tableFromString" $ do
        it "reads a table with one variable" $ do
            let string = unlines ["0 | 1", "1 | 1"]
            let varSet' = Set.fromList [var 0]
            tableFromString varSet' string `shouldBe` allTrueTable varSet'

        it "reads a table with two variables" $ do
            let xorString = unlines ["00 | 0", "01 | 1", "10 | 1", "11 | 0"]
            let varSet' = Set.fromList [var 0, var 1]
            tableFromString varSet' xorString `shouldBe` toTruthTable (Xor [Atom $ var 0, Atom $ var 1])



