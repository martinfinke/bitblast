module TruthBasedGeneticSpec where


import SpecHelper
import TruthBasedGenetic
import TruthBasedCore(Assignment, assignments)
import qualified Data.Map as Map
import qualified System.Random as R
import Control.Monad
import Control.Monad.Random
import Data.List

newtype Candidates = Candidates [Candidate]
    deriving(Show)

instance Arbitrary Candidates where
    arbitrary = do
        numVars <- choose (0,4)
        numExtraVars <- choose (0,3)
        numCandidates <- choose (2,10)
        let as = assignments numVars
        candidates <- replicateM numCandidates $ randomCandidate as numExtraVars
        return . Candidates $ candidates

spec :: Spec
spec = do
    let t = True
    let f = False
    let candidate = Candidate . Map.fromList
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
            property $ \(Candidates (c1@(Candidate m):c2:_)) -> do
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

    -- TODO: Test mutate, mutateAll, randomPlacements


            
