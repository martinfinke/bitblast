module TseitinSelectSpec where

import TseitinSelect

import SpecHelper
import Formula
import Variable
import qualified Data.Set as Set
import Utils(combinationsNoMirror)

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

    describe "possibleReplacementsWith" $ do
        it "doesn't return the root if we don't want it" $ do
            let options = selectOptions{includeRoot=False}
            let f = And [x0]
            possibleReplacementsWith options f `shouldBe` [x0]
        it "doesn't return Atoms if we don't want them" $ do
            let options = selectOptions{includeAtoms=False}
            let f = And [x0]
            possibleReplacementsWith options f `shouldBe` [f]
        it "doesn't return literals (atoms and negated atoms) if we don't want them" $ do
            let options = selectOptions{includeLiterals=False} 
            let f = And [Not x0, x1]
            possibleReplacementsWith options f `shouldBe` [f]
        it "doesn't return the root, even if it's a literal" $ do
            let options = selectOptions{includeRoot=False}
            possibleReplacementsWith options (Not x0) `shouldBe` [x0]
            possibleReplacementsWith options (x0) `shouldBe` []
        it "returns a negated formula, even if includeLiterals is False" $ do
            let options = selectOptions{includeLiterals=False} 
            let f = And [Not $ Implies x0 x2, Not x1]
            possibleReplacementsWith options f `shouldBe` [f, Not $ Implies x0 x2, Implies x0 x2]

    describe "combinationsNoMirror" $ do
        it "works for one element" $ do
            combinationsNoMirror 1 [0] `shouldBe` [[0]]
        it "doesn't do mirrored combinations" $ do
            combinationsNoMirror 2 [0,1] `shouldBe` [[0,1]]
        it "works for length 3" $ do
            combinationsNoMirror 3 [0,1] `shouldBe` [[0,1]]
            combinationsNoMirror 3 [0,1,2] `shouldBe` [[0,1,2]]
            combinationsNoMirror 3 [0,1,2,3] `shouldBe` [[0,1,2],[0,1,3],[0,2,3],[1,2,3]]

    describe "possibleReplacementsNWith" $ do
        let testWith i opts f = possibleReplacementsNWith i opts f
        it "is only the empty list for n=0" $ do
            testWith 0 selectOptions (And []) `shouldBe` [[]]
        it "is the formula for a non-nested formula and n=1" $ do
            testWith 1 selectOptions (And []) `shouldBe` [[And []]]
        it "is is all nodes for n=1" $ do
            let f = And [x0, Or [x1]]
            testWith 1 selectOptions f `shouldBe` [[f], [x0], [Or [x1]], [x1]]
        it "is any combination of two for n=1" $ do
            let f = And [x0, x1]
            let expected = [
                    [And [x0,x1], x0],
                    [And [x0,x1], x1],
                    [x0,x1]
                    ]
            testWith 2 selectOptions f `shouldBe` expected


            