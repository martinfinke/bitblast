module TruthBasedGenetic where

import TruthBasedCore(Assignment, CNF(..), Clause(..), assignments)


import qualified System.Random as R
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

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

fitness :: [Assignment] -> [Assignment] -> Candidate -> IO Int
fitness ones expandedZeros candidate = do
    return 0

optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
optimize numVars f numExtraVars = do
    let as = assignments numVars
    let expansions = assignments numExtraVars
    let (ones, zeros) = partition f as
    let expandedZeros = [zero ++ ex | zero <- zeros, ex <- expansions]
    return $ CNF undefined

