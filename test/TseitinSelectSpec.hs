module TseitinSelectSpec where

import TseitinSelect

import SpecHelper
import Formula
import Variable
import qualified Data.Set as Set

spec :: Spec
spec = do
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars

    describe "possibleReplacements" $ do
        it "returns the atom for a formula that is an atom" $ do
            possibleReplacements x0 `shouldBe` [x0]
        it "returns the negated literal and the atom for a negated literal" $ do
            possibleReplacements (Not x0) `shouldBe` [Not x0, x0]
        it "returns the empty And for an empty And" $ do
            possibleReplacements (And []) `shouldBe` [And []]
        it "returns the And and all of its children" $ do
            let f = And [x0,x1]
            possibleReplacements f `shouldBe` [f,x0,x1]
        it "returns the implication, premise and conclusion" $ do
            let f = Implies x0 x1
            possibleReplacements f `shouldBe` [f,x0,x1]
        it "doesn't return duplicates" $ do
            let child = Not x0
            let parent = Equiv [child, x0, x1]
            let f = And [parent, child]
            possibleReplacements f `shouldBe` [
                    f,
                    parent,
                    child,
                    x0,
                    x1
                    ]
        it "works for nested formulas" $ do
            let grandchild = Equiv [x0,x1]
            let child = Not (Xor [x3,grandchild])
            let parent = Or [child, grandchild]
            let f = And [parent, Not child, Not grandchild]
            possibleReplacements f `shouldBe` [
                    f,
                    parent,
                    child,
                    Xor [x3,grandchild],
                    x3,
                    grandchild,
                    x0,
                    x1,
                    Not child,
                    Not grandchild
                    ]