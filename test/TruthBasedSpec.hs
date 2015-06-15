module TruthBasedSpec where

import SpecHelper
import TruthBased
import Variable
import Formula
import qualified Data.Set as Set

spec :: Spec
spec = do
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars
    describe "expand" $ do
        describe "when given a table with one true output" $ do
            let testVarSet = Set.fromList [v0,v1]
            let testTable = tableFromString testVarSet . unlines $ ["00 | 0", "01 | 0", "10 | 0", "11 | 1"]
            it "returns the input if no extra variables are allowed" $ do
                expand 0 testVarSet testTable `shouldBe` [testTable]
            it "returns 3 tables if 1 extra variable is allowed" $ do
                let withExtraVar = Set.insert v2 testVarSet
                let result = expand 1 testVarSet testTable
                let expected = map (tableFromString withExtraVar . unlines) [
                        ["000 | 0", "001 | 0", "010 | 0", "011 | 0", "100 | 0", "101 | 0", "110 | 0", "111 | 1"],
                        ["000 | 0", "001 | 0", "010 | 0", "011 | 1", "100 | 0", "101 | 0", "110 | 0", "111 | 0"],
                        ["000 | 0", "001 | 0", "010 | 0", "011 | 1", "100 | 0", "101 | 0", "110 | 0", "111 | 1"]
                        ]
                map (truthTableToString withExtraVar) result `shouldBe` map (truthTableToString withExtraVar) expected
                result `shouldBe` expected