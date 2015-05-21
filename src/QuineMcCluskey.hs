module QuineMcCluskey (numRelevantLiterals,
                       groupByRelevantLiterals
                       ) where

import Formula (Formula(..))
import NormalForm
import qualified Data.Map.Lazy as Map
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)

numRelevantLiterals :: Formula -> Int
numRelevantLiterals formula
    | isMinterm formula = let (And literals) = formula in countPositive literals
    | isMaxterm formula = let (Or literals) = formula in countNegative literals
    | otherwise = error $ "Not a min- or maxterm: " ++ show formula
    where countPositive = length . filter isPositiveLiteral
          countNegative = length . filter (not . isPositiveLiteral)

groupByRelevantLiterals :: Formula -> Map.Map Int [Formula]
groupByRelevantLiterals formula
    | not (isCanonical formula) = groupByRelevantLiterals (ensureCanonical formula)
    | isCnf formula = let (And maxterms) = formula in groupTerms maxterms
    | isDnf formula = let (Or minterms) = formula in groupTerms minterms
    | otherwise = error $ "Not a CNF or DNF: " ++ show formula
    
groupTerms :: [Formula] -> Map.Map Int [Formula]
groupTerms = Map.fromList . withNumber . group . sortBy (comparing numRelevantLiterals)
    where group terms = groupBy (\t1 t2 -> numRelevantLiterals t1 == numRelevantLiterals t2) terms :: [[Formula]]
          withNumber = map (\terms@(term:_) -> (numRelevantLiterals term, terms))