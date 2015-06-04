module TseitinSpec where

import SpecHelper
import Formula
import Variable(generateVars)
import Tseitin
import qualified Data.Set as Set

spec :: Spec
spec = do
    let allVars = generateVars 15
    let vars = take 10 allVars
    let [t0,t1,t2,t3,t4] = drop 10 allVars
    let varSet = Set.fromList vars
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars
    let extractMaybe m = case m of
            Just v -> v
            Nothing -> error "Can't extract the value of Nothing"
    describe "extractMaybe" $ do
        it "succeeds for a Just value" $ do
            extractMaybe (Just 1) `shouldBe` 1
        it "throws an error for Nothing" $ do
            evaluate (extractMaybe Nothing) `shouldThrow` anyException
    describe "tseitinReplace" $ do
        it "replaces a given sub-formula with a variable" $ do
            let testFormula = Or [Equiv [x3,x4], x0, And [x6,x2]]
            let (withReplacement,extraVar) = extractMaybe $ tseitinReplace varSet (And [x6,x2]) testFormula
            withReplacement `shouldBe` And [Equiv [Atom extraVar, And [x6,x2]], Or [Equiv [x3,x4], x0, Atom extraVar]]

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
