module TruthBasedGenetic where

import TruthBasedCore(Assignment, CNF(..), Clause(..), assignments, numLiterals)
import EspressoInterface

import qualified System.Random as R
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

-- A candidate maps each f-One-Assignment to a non-empty list of (f' \ f)-Assignments.
newtype Candidate = Candidate (Map.Map Assignment [Assignment])
    deriving(Eq, Show)

merge :: R.RandomGen g => g -> Candidate -> Candidate -> (Candidate, g)
merge rand (Candidate a) (Candidate b)
    | Map.keys a /= Map.keys b = error "merge error: The f-One-Assignments of both candidates have to be the same."
    | otherwise = (Candidate $ Map.fromList mergeResult, newRand)
    where len = length $ Map.keys a
          zipped = zip (Map.toAscList a) (Map.toAscList b)
          combine (aSide,bSide) (accum, rand) =
                let (num, newRand) = R.random rand
                    selection = if (num::Float) < 0.5 then aSide else bSide
                in (selection:accum, newRand)
          (mergeResult, newRand) = foldr combine ([], rand) $ zipped

-- | The lower, the better.
fitness :: Int -> Set.Set Assignment -> [Assignment] -> Candidate -> IO Int
fitness totalNumVars allAssignments ones (Candidate m) = do
    let expandedOnes = Set.fromList $ map expand ones
    let zeros = Set.toList $ allAssignments Set.\\ expandedOnes
    optimized <- espressoOptimize totalNumVars zeros
    return $ numLiterals $ CNF optimized
    where expand one = concatMap (one ++) $ m Map.! one

optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
optimize numVars f numExtraVars = do
    let as = assignments numVars
    let expansions = assignments numExtraVars
    let allAssignments = Set.fromList $ assignments (numVars + numExtraVars)
    let (ones, zeros) = partition f as
    let expandedZeros = [zero ++ ex | zero <- zeros, ex <- expansions]
    let fitness' = fitness (numVars + numExtraVars) allAssignments ones
    return $ CNF undefined

