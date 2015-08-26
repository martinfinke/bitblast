module VariableSpec where

import SpecHelper
import Variable
import Assignment
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

    
            
    
