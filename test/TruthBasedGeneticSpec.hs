module TruthBasedGeneticSpec where


import SpecHelper
import TruthBasedGenetic
import TruthBasedCore(Assignment,assignments,CNF(..),Clause(..),lit)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as R
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe

data Candidates = Candidates [Candidate] Int Int
    deriving(Show)

instance Arbitrary Candidates where
    arbitrary = do
        numVars <- choose (0,4)
        numExtraVars <- choose (0,3)
        numCandidates <- choose (2,10)
        seed <- arbitrary :: Gen Int
        let as = assignments numVars
        let candidates = flip evalRand (R.mkStdGen seed) $ replicateM numCandidates $ randomCandidate as (assignments numExtraVars)
        return $ Candidates candidates numVars numExtraVars

spec :: Spec
spec = do
    let t = True
    let f = False
    let candidate = Candidate . Map.fromList
    let numExtraVars = 3
    let cand1 = candidate [([t,f], [[f,t,f]]), ([f,f], [[f,f,t]])]
    let cand2 = candidate [([t,f], [[t,t,t]]), ([f,f], [[t,f,t]])]
    let cand3 = candidate [([t,f], [[f,f,f]]), ([f,f], [[t,t,f], [f,t,t]])]
    let rand = R.mkStdGen 15
    let eval = flip evalRand rand
    describe "merge" $ do
        let candInvalid = candidate [([t,f,f], [])]
        it "works with empty candidates" $ do
            let empty = candidate []
            let result = eval $ merge empty empty
            result `shouldBe` empty
        it "selects assignments from both (given a suitable random gen)" $ do
            let result = eval $ merge cand1 cand2
            result `shouldBe` candidate [([t,f], [[t,t,t]]), ([f,f], [[f,f,t]])]
        it "throws an error when candidates are for different f-One-Assignments" $ do
            let result = eval $ merge cand1 candInvalid
            evaluate result `shouldThrow` anyErrorCall
        it "always returns a new candidate with the same numVars as the operands" $ do
            property $ \(Candidates (c1@(Candidate m):c2:_) _ _) -> do
                Candidate m' <- evalRandIO $ merge c1 c2
                let numVars = length . head . Map.keys
                numVars m' `shouldBe` numVars m

    describe "mergeAll" $ do
        it "merges 3 candidates pairwise into 3 new candidates" $ do
            let cs = [cand1,cand2,cand3]
            let rand = R.mkStdGen 17
            let result = flip evalRand rand $ mergeAll cs
            length result `shouldBe` 3
            result `shouldBe` map candidate [
                [([f,f], [[t,t,f], [f,t,t]]), ([t,f], [[f,t,f]])],
                [([f,f], [[t,f,t]]), ([t,f], [[t,t,t]])],
                [([f,f], [[t,f,t]]), ([t,f], [[f,f,f]])]
                ]

    describe "randomPlacements" $ do
        it "returns different results for different RNGs" $ do
            let rand1 = R.mkStdGen 17
            let rand2 = R.mkStdGen 1736
            let expansions = assignments 5
            let place = randomPlacements expansions 4
            evalRand place rand1 `shouldNotBe` evalRand place rand2

    describe "mutate" $ do
        it "never changes when amount is 0" $ do
            property $ \(Candidates (cand@(Candidate m):_) _ numExtraVars) seed ->
                let keys = Map.keys m
                    expansions = assignments numExtraVars
                    rand = R.mkStdGen seed
                    Candidate m' = flip evalRand rand $ mutate expansions 0 cand
                in forM_ keys $ \key -> do
                    let lookup = fromJust . Map.lookup key
                    lookup m' `shouldBeSetEqualTo` lookup m

    describe "mutateAll" $ do
        let rand = R.mkStdGen 1290
        it "randomly changes a candidate" $ do
            let result = head $ evalRand (mutateAll numExtraVars 0.3 [cand3]) rand
            result `shouldBe` candidate [([f,f],[[f,t,t],[t,t,f],[t,f,t]]),([t,f],[[f,f,f]])]
        it "always places at least a one" $ do
            property $ \(Candidates (cand@(Candidate m):_) _ numExtraVars) (OneHundredOrLess seed) ->
                let rand = R.mkStdGen seed
                    Candidate m' = head $ evalRand (mutateAll numExtraVars 0.5 [cand]) rand
                    keys = Map.keys m
                in do
                    Map.keys m' `shouldBeSetEqualTo` keys
                    forM_ keys $ \key -> do
                        Map.lookup key m' `shouldNotBe` Nothing
                        Map.lookup key m' `shouldNotBe` Just []

    describe "randomCandidate" $ do
        it "creates different candidates for two different RNGs" $ do
            let ones = [[t, f], [f, t]]
            let expansions = assignments 3
            let [cand1,cand2] = map (evalRand $ randomCandidate expansions ones) [R.mkStdGen 17, R.mkStdGen 1737]
            cand1 `shouldNotBe` cand2
            getOnes cand1 `shouldBeSetEqualTo` ones
            getOnes cand2 `shouldBeSetEqualTo` ones
        it "always places at least a one" $ do
            property $ \(OneHundredOrLess seed) (TenOrLess i) ->
                let rand = R.mkStdGen seed
                    ones = [[f,f], [t,f]]
                    expansions = assignments $ min 5 i
                    Candidate m = evalRand (randomCandidate expansions ones) rand
                in forM_ ones $ \one -> do
                    Map.lookup one m `shouldNotBe` Nothing
                    Map.lookup one m `shouldNotBe` Just []

    describe "generation" $ do
        it "never grows or shrinks" $ do
            property $ \(Candidates cs _ numExtraVars) ->
                let rand = R.mkStdGen 15
                    new = evalRand (generation defaultOptions numExtraVars cs) rand
                in length new `shouldBe` length cs

    describe "candidateToCNF" $ do
        let totalNumVars = 3
        it "converts a candidate correctly" $ do
            let cand = candidate [([t,f], [[f]]), ([f,f], [[t], [f]])]
            candidateToCNF totalNumVars cand `shouldBe` CNF [
                Clause [lit 1 f, lit 2 t, lit 3 f],
                Clause [lit 1 f, lit 2 t, lit 3 t],
                Clause [lit 1 t, lit 2 f, lit 3 t],
                Clause [lit 1 t, lit 2 t, lit 3 f],
                Clause [lit 1 t, lit 2 t, lit 3 t]
                ]


    -- TODO: Test fitness function. Why does this candidate have fitness == 0?
    -- Candidate (fromList [([False,False,True],[[True],[False]]),([False,True,False],[[True],[False]]),([True,False,False],[[False],[True]]),([True,True,True],[[False],[True]])])


            
