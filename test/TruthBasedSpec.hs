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
            let testTable = tableFromList $ zip (allBoolCombinations (Set.fromList [v0,v1])) [False, False, False, True]
            it "returns the input if no extra variables are allowed" $ do
                expand 0 testTable `shouldBe` [testTable]
            it "returns 3 tables if 1 extra variable is allowed" $ do
                pending -- TODO
                --expand 1 testTable `shouldBe` []