module QuineMcCluskey where

import Formula
import qualified Data.Map.Lazy as Map
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)
import Debug.Trace(traceShow)

-- TODO: We should convert to the canonical form first!
numRelevantLiterals :: Formula -> Int
numRelevantLiterals formula
    | isMinterm formula = let (And literals) = formula in countPositive literals
    | isMaxterm formula = let (Or literals) = formula in countNegative literals
    | otherwise = error $ "Not a min- or maxterm: " ++ show formula
    where countPositive = length . filter isPositiveLiteral
          countNegative = length . filter (not . isPositiveLiteral)

groupByRelevantLiterals :: Formula -> Map.Map Int [Formula]
groupByRelevantLiterals formula
    | isCnf formula = let (And maxterms) = formula in toMap maxterms
    | isDnf formula = let (Or minterms) = formula in toMap minterms
    | otherwise = error $ "Not a CNF or DNF: " ++ show formula
    where group terms = groupBy (\t1 t2 -> numRelevantLiterals t1 == numRelevantLiterals t2) terms :: [[Formula]]
          withNumber = map (\terms@(term:_) -> (numRelevantLiterals term, terms))
          toMap = Map.fromList . withNumber . group . sortBy (comparing numRelevantLiterals)