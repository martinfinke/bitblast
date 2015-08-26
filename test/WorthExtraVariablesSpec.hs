module WorthExtraVariablesSpec where

import SpecHelper
import WorthExtraVariables
import Variable
import TruthTable
import qualified Data.Set as Set

spec :: Spec
spec = do
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars

    describe "allPossibleTables" $ do
        it "generates all possible truth tables with one variable" $ do
            let varSet = Set.fromList [v0]
            allPossibleTables varSet `shouldBe` map (tableFromString varSet . unlines) [
                    ["0 | 0", "1 | 0"],
                    ["0 | 1", "1 | 0"],
                    ["0 | 0", "1 | 1"],
                    ["0 | 1", "1 | 1"]
                    ]
        it "generates all possible truth tables with two variables" $ do
            let varSet = Set.fromList [v0, v1]
            Set.fromList (allPossibleTables varSet) `shouldBe` Set.fromList (map (tableFromString varSet . unlines) [
                    ["00 | 0", "01 | 0", "10 | 0", "11 | 0"],
                    ["00 | 0", "01 | 0", "10 | 0", "11 | 1"],
                    ["00 | 0", "01 | 0", "10 | 1", "11 | 0"],
                    ["00 | 0", "01 | 0", "10 | 1", "11 | 1"],
                    ["00 | 0", "01 | 1", "10 | 0", "11 | 0"],
                    ["00 | 0", "01 | 1", "10 | 0", "11 | 1"],
                    ["00 | 0", "01 | 1", "10 | 1", "11 | 0"],
                    ["00 | 0", "01 | 1", "10 | 1", "11 | 1"],
                    ["00 | 1", "01 | 0", "10 | 0", "11 | 0"],
                    ["00 | 1", "01 | 0", "10 | 0", "11 | 1"],
                    ["00 | 1", "01 | 0", "10 | 1", "11 | 0"],
                    ["00 | 1", "01 | 0", "10 | 1", "11 | 1"],
                    ["00 | 1", "01 | 1", "10 | 0", "11 | 0"],
                    ["00 | 1", "01 | 1", "10 | 0", "11 | 1"],
                    ["00 | 1", "01 | 1", "10 | 1", "11 | 0"],
                    ["00 | 1", "01 | 1", "10 | 1", "11 | 1"]
                    ])
