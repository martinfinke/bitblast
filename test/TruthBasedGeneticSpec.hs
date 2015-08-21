module TruthBasedGeneticSpec where


import SpecHelper
import TruthBasedGenetic
import TruthBasedCore(Assignment, assignments)
import qualified Data.Map as Map
import qualified System.Random as R
import Control.Monad
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

randomCandidate :: [Assignment] -> Int -> Gen Candidate
randomCandidate assignments numExtraVars = do
    selections <- replicateM (length assignments) $ do
        maxNumPlacements <- choose (1, 2^numExtraVars)
        fmap nub $ replicateM maxNumPlacements $ vectorOf numExtraVars (arbitrary::Gen Bool)
    return . Candidate $ Map.fromList $ zip assignments selections

spec :: Spec
spec = do
    describe "merge" $ do
        let rand = R.mkStdGen 15
        let t = True
        let f = False
        let candidate = Candidate . Map.fromList
        let cand1 = candidate [([t,f], [[f,t,f]]), ([f,f], [[f,f,t]])]
        let cand2 = candidate [([t,f], [[t,t,t]]), ([f,f], [[t,f,t]])]
        let candInvalid = candidate [([t,f,f], [])]
        it "works with empty candidates" $ do
            let empty = candidate []
            fst (merge rand empty empty) `shouldBe` empty
        it "selects assignments from both (given a suitable random gen)" $ do
            fst (merge rand cand1 cand2) `shouldBe` candidate [([t,f], [[f,t,f]]), ([f,f], [[t,f,t]])]
        it "throws an error when candidates are for different f-One-Assignments" $ do
            evaluate (merge rand cand1 candInvalid) `shouldThrow` anyErrorCall
        it "always returns a new candidate with the same numVars as the operands" $ do
            property $ \(Candidates (c1@(Candidate m):c2:_)) -> do
                rand <- R.getStdGen
                let result@(Candidate m') = fst $ merge rand c1 c2
                let numVars = length . head . Map.keys
                numVars m' `shouldBe` numVars m
