module QuineMcCluskey (numRelevantLiterals,
                       groupByRelevantLiterals,
                       neighbourKeys,
                       neighbourTerms
                       ) where

import Formula (Formula(..))
import NormalForm
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)

numRelevantLiterals :: Formula -> Int
numRelevantLiterals formula
    | isConjunctionOfLiterals formula = countPositive $ normalFormChildren formula
    | isDisjunctionOfLiterals formula = countNegative $ normalFormChildren formula
    | otherwise = error $ "Not a conjunction/disjunction of literals: " ++ show formula
    where countPositive = length . filter isPositiveLiteral
          countNegative = length . filter (not . isPositiveLiteral)

groupByRelevantLiterals :: Formula -> Map.Map Int [Formula]
groupByRelevantLiterals formula
    | not (isCanonical formula) = groupByRelevantLiterals (ensureCanonical formula)
    | isCnf formula || isDnf formula = groupTerms $ normalFormChildren formula
    | otherwise = error $ "Not a CNF or DNF: " ++ show formula
    
groupTerms :: [Formula] -> Map.Map Int [Formula]
groupTerms = Map.fromList . withNumber . group . sortBy (comparing numRelevantLiterals)
    where group terms = groupBy (\t1 t2 -> numRelevantLiterals t1 == numRelevantLiterals t2) terms :: [[Formula]]
          withNumber = map (\terms@(term:_) -> (numRelevantLiterals term, terms))

neighbourKeys :: [Int] -> [(Int, Int)]
neighbourKeys ints = [(i,i+1) | i <- ints, (i+1) `elem` ints]

partitionTerms :: Map.Map Int [Formula] -> ([Formula], [Formula])
partitionTerms termMap = (primeTerms, mergedTerms)
    where primeTerms = undefined
          mergedTerms = undefined
          

neighbourTerms :: Map.Map Int [Formula] -> [(Formula, Formula)]
neighbourTerms termMap = filter ((== 1) . hammingDistance) $ concatMap allTermPairsForKeyPair neighbourKeyPairs
    where neighbourKeyPairs = neighbourKeys $ Map.keys termMap :: [(Int, Int)]
          allTermPairsForKeyPair (key1,key2) = [(term1,term2) | term1 <- lookupOrEmptyList key1, term2 <- lookupOrEmptyList key2]
          lookupOrEmptyList key = maybe [] id (Map.lookup key termMap)

-- | The terms have to be min/maxterms.
hammingDistance :: (Formula, Formula) -> Int
hammingDistance (term1, term2) = Set.size diff
    where diff = Set.difference (literals term1) (literals term2)
          literals = Set.fromList . normalFormChildren
