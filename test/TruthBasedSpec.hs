module TruthBasedSpec where

import SpecHelper
import TruthBased
import TruthBasedCore
import Variable
import Formula
import TruthBasedCoreSpec(cls,cnf)

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


            