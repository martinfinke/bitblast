module AssignmentSpec where

import Assignment
import SpecHelper
import Variable(var)
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Assignment get/setVar" $ do
        it "is Nothing for an empty assignment" $ do
            getVar (var 0) emptyAssignment `shouldBe` Nothing
        it "is Just True for an assignment where the variable was set to True" $ do
            getVar (var 0) (setVar (var 0) True emptyAssignment) `shouldBe` Just True
        it "is Nothing for an assignment where another variable was set" $ do
            getVar (var 1) (setVar (var 0) True emptyAssignment) `shouldBe` Nothing
        it "is equal for two assignments where the same variable was set to the same value" $ do
            let a1 = setVar (var 0) True emptyAssignment
            let a2 = setVar (var 0) True emptyAssignment
            a1 `shouldBe` a2
        it "is not equal for two assignments where the same variable was set to a different value" $ do
            let a1 = setVar (var 0) True emptyAssignment
            let a2 = setVar (var 0) False emptyAssignment
            (a1 == a2) `shouldBe` False

    describe "assignmentFromList" $ do
        it "is the empty assignment for an empty list" $ do
            assignmentFromList [] `shouldBe` emptyAssignment
        it "is the same as setting variables using setVar" $ do
            assignmentFromList [((var 0), True)] `shouldBe` setVar (var 0) True emptyAssignment
        it "doesn't set variables that aren't in the list" $ do
            let a = assignmentFromList [((var 1), False)]
            getVar (var 0) a `shouldBe` Nothing

    describe "assignmentFromString" $ do
        it "is the emptyAssignment for an empty string" $ do
            assignmentFromString Set.empty "" `shouldBe` emptyAssignment
        it "throws an error if the variable set doesn't match the string" $ do
            evaluate (assignmentFromString (Set.singleton (var 0)) "01") `shouldThrow` anyException
        it "works for one variable" $ do
            getVar (var 1) (assignmentFromString (Set.singleton (var 1)) "1") `shouldBe` Just True
        it "doesn't assign other variables" $ do
            getVar (var 1) (assignmentFromString (Set.singleton (var 0)) "1") `shouldBe` Nothing
        it "expects the last-added variable on the left end" $ do
            let assignment = (assignmentFromString (Set.fromList [(var 1),(var 0)]) "10")
            getVar (var 1) assignment `shouldBe` Just True
            getVar (var 0) assignment `shouldBe` Just False

    describe "assignmentToString" $ do
        it "is the empty string for an empty assignment" $ do
            assignmentToString Set.empty emptyAssignment `shouldBe` ""
        it "creates a dash if a variable isn't in the assignment" $ do
            assignmentToString (Set.singleton (var 0)) emptyAssignment `shouldBe` "-"
            assignmentToString (Set.fromList [(var 0),(var 1),(var 2)]) emptyAssignment `shouldBe` "---"
        it "creates a one for a true value" $ do
            assignmentToString (Set.singleton (var 0)) (setVar (var 0) True emptyAssignment) `shouldBe` "1"
        it "creates a zero for a false value" $ do
            assignmentToString (Set.fromList [(var 0),(var 1),(var 2)]) (setVar (var 0) False emptyAssignment) `shouldBe` "--0"

    describe "expandOrReduce" $ do
        it "does nothing if the variableSet is the same as in the assignment" $ do
            let assignment = setVar (var 0) True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [(var 0)]) assignment
            assignment' `shouldBe` assignment
        it "reduces to an emptyAssignment if the variableSet is empty" $ do
            let assignment = setVar (var 0) True emptyAssignment
            let shouldBeEmpty = expandOrReduce True Set.empty assignment
            shouldBeEmpty `shouldBe` emptyAssignment
        it "removes a variable that's not in the variable set" $ do
            let assignment = setVar (var 0) True $ setVar (var 1) False emptyAssignment
            let assignment' = expandOrReduce True (Set.fromList [(var 1)]) assignment
            assignment' `shouldBe` setVar (var 1) False emptyAssignment
        it "assigns a variable that's not yet in the assignment" $ do
            let assignment = setVar (var 0) True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [(var 0), (var 1)]) assignment
            let expected = setVar (var 0) True $ setVar (var 1) False emptyAssignment
            assignment' `shouldBe` expected
        it "can add and remove at the same time" $ do
            let assignment = setVar (var 0) False $ setVar (var 1) True emptyAssignment
            let assignment' = expandOrReduce False (Set.fromList [(var 1), (var 2)]) assignment
            let expected = setVar (var 2) False $ setVar (var 1) True emptyAssignment
            assignment' `shouldBe` expected


    describe "allBoolCombinations" $ do
        it "returns two combinations for one variable" $ do
            let varSet = Set.fromList [var 0]
            allBoolCombinations varSet `shouldBe` map (assignmentFromString varSet) ["0", "1"]
        it "returns four combinations for two variables" $ do
            let varSet = Set.fromList [var 0,var 3]
            allBoolCombinations varSet `shouldBe` map (assignmentFromString varSet) ["00", "01", "10", "11"]

    describe "merge" $ do
        let varSet1 = Set.fromList [var 0,var 1,var 2]
        let varSet2 = Set.fromList [var 3,var 4]
        let test s1 s2 expected = merge (assignmentFromString varSet1 s1) (assignmentFromString varSet2 s2) `shouldBe` assignmentFromString (Set.union varSet1 varSet2) expected
        it "merges two assignments" $ do
            test "001" "11" "11001"
            test "111" "00" "00111"
        it "merges two assignments that have no overlap" $ do
            let varSet = Set.fromList [var 2,var 5,var 6]
            let a1 = assignmentFromString (Set.fromList [var 2]) "1"
            let a2 = assignmentFromString (Set.fromList [var 5,var 6]) "10"
            merge a1 a2 `shouldBe` assignmentFromString varSet "101"

            
