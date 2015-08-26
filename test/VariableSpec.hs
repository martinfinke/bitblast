module VariableSpec where

import SpecHelper
import Variable
import Formula
import Control.Monad(forM)
import qualified Data.Set as Set

instance Arbitrary Variable where
    arbitrary = do
        candidates <- randomVariables 10
        elements candidates

randomVariables :: Int -> Gen [Variable]
randomVariables numvars = do
    return $ makeVars numvars

instance Arbitrary Assignment where
    arbitrary = do
        numvars <- choose (30,100::Int)
        variables <- randomVariables numvars
        bools <- vectorOf numvars (arbitrary::Gen Bool)
        return $ assignmentFromList (zip variables bools)



spec :: Spec
spec = do
    let [test1,test2,test3] = makeVars 3
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars

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
            assignmentFromString Set.empty "" `shouldBe` emptyAssignment
        it "throws an error if the variable set doesn't match the string" $ do
            evaluate (assignmentFromString (Set.singleton test1) "01") `shouldThrow` anyException
        it "works for one variable" $ do
            getVar test2 (assignmentFromString (Set.singleton test2) "1") `shouldBe` Just True
        it "doesn't assign other variables" $ do
            getVar test2 (assignmentFromString (Set.singleton test1) "1") `shouldBe` Nothing
        it "expects the last-added variable on the left end" $ do
            let assignment = (assignmentFromString (Set.fromList [test2,test1]) "10")
            getVar test2 assignment `shouldBe` Just True
            getVar test1 assignment `shouldBe` Just False

    describe "assignmentToString" $ do
        it "is the empty string for an empty assignment" $ do
            assignmentToString Set.empty emptyAssignment `shouldBe` ""
        it "creates a dash if a variable isn't in the assignment" $ do
            assignmentToString (Set.singleton test1) emptyAssignment `shouldBe` "-"
            assignmentToString (Set.fromList [test1,test2,test3]) emptyAssignment `shouldBe` "---"
        it "creates a one for a true value" $ do
            assignmentToString (Set.singleton test1) (setVar test1 True emptyAssignment) `shouldBe` "1"
        it "creates a zero for a false value" $ do
            assignmentToString (Set.fromList [test1,test2,test3]) (setVar test1 False emptyAssignment) `shouldBe` "--0"

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

    describe "tableFromString" $ do
        it "reads a table with one variable" $ do
            let string = unlines ["0 | 1", "1 | 1"]
            let varSet' = Set.fromList [v0]
            tableFromString varSet' string `shouldBe` allTrueTable varSet'

        it "reads a table with two variables" $ do
            let xorString = unlines ["00 | 0", "01 | 1", "10 | 1", "11 | 0"]
            let varSet' = Set.fromList [v0, v1]
            tableFromString varSet' xorString `shouldBe` toTruthTable (Xor [Atom v0, Atom v1])

    describe "merge" $ do
        let varSet1 = Set.fromList [v0,v1,v2]
        let varSet2 = Set.fromList [v3,v4]
        let test s1 s2 expected = merge (assignmentFromString varSet1 s1) (assignmentFromString varSet2 s2) `shouldBe` assignmentFromString (Set.union varSet1 varSet2) expected
        it "merges two assignments" $ do
            test "001" "11" "11001"
            test "111" "00" "00111"

    describe "allBoolCombinations" $ do
        it "returns two combinations for one variable" $ do
            let varSet = Set.fromList [v0]
            allBoolCombinations varSet `shouldBe` map (assignmentFromString varSet) ["0", "1"]
        it "returns four combinations for two variables" $ do
            let varSet = Set.fromList [v0,v3]
            allBoolCombinations varSet `shouldBe` map (assignmentFromString varSet) ["00", "01", "10", "11"]

    describe "merge" $ do
        it "merges two assignments that have no overlap" $ do
            let varSet = Set.fromList [v2,v5,v6]
            let a1 = assignmentFromString (Set.fromList [v2]) "1"
            let a2 = assignmentFromString (Set.fromList [v5,v6]) "10"
            merge a1 a2 `shouldBe` assignmentFromString varSet "101"

            
