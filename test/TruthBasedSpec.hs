module TruthBasedSpec where

import SpecHelper
import TruthBased
import Variable
import Formula
import NormalForm
import qualified Data.Set as Set
import Data.List(sort)
import BranchTruthTables

spec :: Spec
spec = do
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars
    describe "expand" $ do
        describe "when given a table with two variables and one true output" $ do
            let testVarSet = Set.fromList [v0,v1]
            let testTable = tableFromString testVarSet . unlines $ ["00 | 0", "01 | 0", "10 | 0", "11 | 1"]
            it "returns the input if no extra variables are allowed" $ do
                fst (expand' 0 testVarSet testTable) `shouldBe` [testTable]
            it "returns 3 tables if 1 extra variable is allowed" $ do
                let withExtraVar = Set.insert v2 testVarSet
                let result = fst $ expand' 1 testVarSet testTable
                let expected = map (tableFromString withExtraVar . unlines) twoVars_oneOne_oneExtra
                result `shouldBe` expected
        describe "when given a table with one variable and one true output" $ do
            it "returns 15 tables if 2 extra variables are allowed" $ do
                let testVarSet = Set.singleton v0
                let smallTable = tableFromString testVarSet . unlines $ ["0 | 1", "1 | 0"]
                let withExtraVars = Set.insert v2 $ Set.insert v1 testVarSet
                let result = sort . fst $ expand' 2 testVarSet smallTable
                let expected = sort $ map (tableFromString withExtraVars . unlines) oneVar_oneOne_twoExtra
                result `shouldBe` expected
        describe "when given a table with two ones" $ do
            it "returns x tables if 2 extra variables are allowed" $ do
                let testVarSet = Set.singleton v0
                let smallTable = tableFromString testVarSet . unlines $ ["0 | 1", "1 | 1"]
                let withExtraVars = Set.insert v2 $ Set.insert v1 testVarSet
                let result = sort . fst $ expand' 2 testVarSet smallTable
                let expected  = sort $ map (tableFromString withExtraVars . unlines) oneVar_twoOnes_twoExtra
                result `shouldBe` expected


    describe "possibleFormulas" $ do
        it "returns only one CNF if no extra variables are allowed" $ do
            let f = Equiv [x0, Not x0]
            map fst (possibleCnfs 0 f) `shouldBe` [toCanonicalCnf f]
        it "returns three CNFs for a formula with one true output, and one allowed extra variable" $ do
            let f = x0
            let possibles = map fst $ possibleCnfs 1 f
            let expected = map ensureCanonical [
                    And [Or [x0, x1], Or [x0, Not x1], Or [Not x0, x1]],
                    And [Or [x0, x1], Or [x0, Not x1], Or [Not x0, Not x1]],
                    And [Or [x0, x1], Or [x0, Not x1]]
                    ]
            possibles `shouldBe` expected

    describe "atLeastOne" $ do
        it "returns nothing for the empty list" $ do
            atLeastOne ([]::[Int]) `shouldBe` []
        it "returns one combination for a list with one element" $ do
            atLeastOne [17] `shouldBe` [[17]]
        it "returns three combinations for a list with two elements" $ do
            atLeastOne [4,7] `shouldBe` [[4], [7], [4,7]]

    describe "combinations" $ do
        it "returns just one table if no extra variables are allowed" $ do
            let oldVarSet = Set.fromList [v0,v1]
            let trues = map (assignmentFromString oldVarSet) ["00", "10"]
            combinations oldVarSet Set.empty trues `shouldBe` [(tableFromString oldVarSet . unlines) [
                    "00 | 1", "01 | 0", "10 | 1", "11 | 0"
                    ]]