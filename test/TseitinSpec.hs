module TseitinSpec where

import SpecHelper
import FormulaSpec
import Formula
import Variable(makeVars)
import Tseitin
import qualified Data.Set as Set

spec :: Spec
spec = do
    let allVars = makeVars 15
    let vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = take 10 allVars
    let tseitinVars@[t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars


    describe "findAndReplace" $ do
        it "replaces a positive literal" $ do
            findAndReplace x0 t0 x0 `shouldBe` (True, Atom t0)
        it "replaces a negated literal" $ do
            findAndReplace (Not x0) t0 (Not x0) `shouldBe` (True, Atom t0)
        it "replaces an atom inside a negation" $ do
            findAndReplace x0 t0 (Not x0) `shouldBe` (True, Not $ Atom t0)
        it "replaces an atom inside an AND" $ do
            findAndReplace x0 t0 (And [x0,x1]) `shouldBe` (True, And [Atom t0,x1])
        it "replaces an atom inside a premise" $ do
            findAndReplace x1 t0 (Implies x1 x2) `shouldBe` (True, Implies (Atom t0) x2)
        it "replaces an atom inside a conclusion" $ do
            findAndReplace x2 t0 (Implies x1 x2) `shouldBe` (True, Implies x1 (Atom t0))
        it "replaces multiple instances at the same level" $ do
            findAndReplace x2 t0 (And [x1,Not x2,x3,x2]) `shouldBe` (True, And [x1,Not $ Atom t0,x3,Atom t0])
        it "replaces multiple instances at different levels" $ do
            findAndReplace x2 t0 (And [x1,x2,Xor [x5,Not x2]]) `shouldBe` (True, And [x1,Atom t0,Xor [x5,Not $ Atom t0]])
        it "doesn't replace anything if the search term can't be found" $ do
            findAndReplace x5 t0 (And [x1,x2,x3]) `shouldBe` (False, And [x1,x2,x3])
        it "replaces an AND inside an OR" $ do
            let toReplace = And [x2,x3]
            findAndReplace toReplace t0 (Or [toReplace]) `shouldBe` (True, Or [Atom t0])

    describe "isChildOf" $ do
        it "is True if both formulas are the same" $ do
            property $ \formula ->
                formula `isChildOf` formula `shouldBe` True
        it "is False if the formulas are unrelated" $ do
            let f1 = And [x0,x3, Xor [x2]]
            let f2 = And [x0, x3, x2, Xor [x1]]
            f1 `isChildOf` f2 `shouldBe` False
            f2 `isChildOf` f1 `shouldBe` False
        it "is False if the formulas have a common subtree" $ do
            let common = Xor [x1]
            let f1 = And [x0,x3, common]
            let f2 = And [x0, x3, x2, common]
            f1 `isChildOf` f2 `shouldBe` False
            f2 `isChildOf` f1 `shouldBe` False
        it "is true if one is the child of the other" $ do
            let f1 = Xor [And [x0,x2]]
            let f2 = Or [x5, f1]
            f1 `isChildOf` f2 `shouldBe` True
            f2 `isChildOf` f1 `shouldBe` False

    describe "containsOnlyVarsFrom" $ do
        it "is True if it contains only vars from the set" $ do
            And [x0,x1] `containsOnlyVarsFrom` varSet `shouldBe` True
        it "is False if it contains vars not in the set" $ do
            And [x0,x3] `containsOnlyVarsFrom` (Set.fromList [v0,v1]) `shouldBe` False
        it "is True if it contains a (strict) subset" $ do
            And [x0,x3] `containsOnlyVarsFrom` (Set.fromList [v0,v1,v3]) `shouldBe` True
    
    describe "normalize" $ do
        it "does nothing to an empty group" $ do
            normalize [] `shouldBe` []
        it "does nothing to a group with only one element" $ do
            let group = [(And [x0],t0)]
            normalize group `shouldBe` group
        it "normalizes a group with a parent and a child" $ do
            let child = And [x1, x2]
            let parent = Or [Implies x0 child]
            let group = [(child,t0), (parent,t1)]
            normalize group `shouldBe` [(child,t0), (Or [Implies x0 $ Atom t0], t1)]
        it "works for a chain of 3 related formulas" $ do
            let grandchild = Equiv [x0]
            let child = And [x1, grandchild]
            let parent = Or [Implies x0 child]
            let group = [(grandchild,t0), (child,t1), (parent,t2)]
            let expected = [(grandchild,t0), (And [x1, Atom t0], t1), (Or [Implies x0 $ Atom t1], t2)]
            normalize group `shouldBe` expected
        it "works when parent contains grandchild and child" $ do
            let grandchild = Equiv [x0]
            let child = And [x1, grandchild]
            let parent = Or [x0, Implies grandchild child, grandchild]
            let group = [(grandchild,t0), (child,t1), (parent,t2)]
            let expected = [(grandchild,t0), (And [x1, Atom t0], t1), (Or [x0, Implies (Atom t0) (Atom t1), Atom t0], t2)]
            normalize group `shouldBe` expected
        it "works when one term normalizes to the other" $ do
            let term = And [x1,x2]
            let group = [(term,t0), (term,t1)]
            normalize group `shouldBe` [(term,t0), (Atom t0, t1)]

    describe "tseitinReplace" $ do
        it "doesn't change anything if there are no replaces" $ do
            let f = And [x0,x1]
            tseitinReplace varSet [] f `shouldBe` (f, [])
        it "throws an error if a variable isn't in the variable set" $ do
            let toReplace = Atom t0
            let call = tseitinReplace varSet [toReplace] x1
            evaluate call `shouldThrow` anyException
        it "replaces a given sub-formula with a variable" $ do
            let testFormula = Or [Equiv [x3,x4], x0, And [x6,x2]]
            let (withReplacement,[extraVar]) = tseitinReplace varSet [And [x6,x2]] testFormula
            withReplacement `shouldBe` And [Or [Equiv [x3,x4], x0, Atom extraVar], Equiv [Atom extraVar, And [x6,x2]]]
        it "can replace multiple occurrences" $ do
            let toReplace = Not x4
            let f = And [toReplace, x1, Equiv [x2, toReplace]]
            let result@(_,[newVar]) = tseitinReplace varSet [toReplace] f
            let expected = And [And [Atom newVar, x1, Equiv [x2, Atom newVar]], Equiv [Atom newVar, Not x4]]
            result `shouldBe` (expected,[newVar])
        it "doesn't use variables from the given varSet" $ do
            let f = And [x0]
            let (_,[extraVar]) = tseitinReplace varSet [x0] f
            Set.member extraVar varSet `shouldBe` False
        it "can replace parent/child/grandchild, even if they are given in a wrong order" $ do
            let grandchild = x0
            let child = Or [grandchild, Implies x4 grandchild]
            let parent = And [x2,child]
            let unrelated1 = Or [x4]
            let toReplaces = [child, unrelated1, parent, grandchild]
            let f = Xor [child, x1, grandchild, Or [Not child, unrelated1, x2], Not parent]
            let (result,[t0',t1',t2',t3']) = tseitinReplace varSet toReplaces f
            let replaced = Xor [Atom t1', x1, Atom t0', Or [Not (Atom t1'), Atom t2', x2], Not (Atom t3')]
            let equivs = [
                    Equiv [Atom t0', x0],
                    Equiv [Atom t1', Or [Atom t0', Implies x4 (Atom t0')]],
                    Equiv [Atom t2', unrelated1],
                    Equiv [Atom t3', And [x2, Atom t1']]
                    ]
            let expected = And (replaced:equivs)
            result `shouldBe` expected
            