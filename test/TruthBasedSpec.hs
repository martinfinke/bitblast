module TruthBasedSpec where

import SpecHelper
import TruthBased
import TruthBasedCore
import Variable
import Formula
import TruthBasedCoreSpec(cls,cnf)
import QmcTypes

import qualified Data.Map as Map

spec :: Spec
spec = do
    let vars@[v0,v1,v2,v3] = makeVars 4
    let [x0,x1,x2,x3] = map Atom vars
    describe "convertLiteral" $ do
        let mapping = Map.fromList $ zip variableNumbers vars
        it "converts 1 to x0" $ do
            convertLiteral mapping (lit 1 True) `shouldBe` x0
        it "converts -1 to Not x0" $ do
            convertLiteral mapping (lit 1 False) `shouldBe` Not x0
        it "converts 2 to x1" $ do
            convertLiteral mapping (lit 2 True) `shouldBe` x1

    describe "toCoreFormula" $ do
        it "converts a Formula into a function" $ do
            let formula = And [x0,x1,Not x2,x3]
            let function = toCoreFormula vars formula
            function [True, True, False, True] `shouldBe` True
            function [True, True, False, False] `shouldBe` False
            function [True, False, False, True] `shouldBe` False
            function [True, True, True, True] `shouldBe` False

    describe "fromCoreCNF" $ do
        it "converts a Core.CNF to a Formula" $ do
            let expected = And [Or [x0, Not x2], Or [x1, x3]]
            fromCoreCNF vars (cnf [[1, -3], [2, 4]]) `shouldBe` expected

    describe "toBitVector" $ do
        it "converts the all false assignment to a 0" $ do
            toBitVector [False, False, False] `shouldBe` 0
        it "converts a true assignment with 1 variable to 1" $ do
            toBitVector [True] `shouldBe` 1
        it "converts mixed assignments correctly" $ do
            toBitVector [True, False] `shouldBe` 1
            toBitVector [False, True] `shouldBe` 2
            toBitVector [True, True] `shouldBe` 3
            toBitVector [False, False, True] `shouldBe` 4
            toBitVector [True, False, True] `shouldBe` 5
            toBitVector [False, True, True] `shouldBe` 6
            toBitVector [True, True, True] `shouldBe` 7

    describe "fromQmTerm" $ do
        it "converts a 0/0 term to " $ do
            fromQmTerm 4 (fromString "----") `shouldBe` Clause []
        it "converts a true assignment with 1 variable to a clause" $ do
            fromQmTerm 3 (bitVectorToQmTerm $ toBitVector [False, False, True]) `shouldBe` Clause (map Lit [1,2,-3])

            