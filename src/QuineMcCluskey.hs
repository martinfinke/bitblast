module QuineMcCluskey (QmcTerm(..),
                       fromString,
                       formulaToQmcTerms,
                       termToQmcTerm,
                       valueForVariableIndex,
                       quineMinterms,
                       quineMaxterms,
                       numRelevantLiterals',
                       numRelevantLiterals,
                       groupByRelevantLiterals,
                       hammingDistance,
                       neighbourKeys,
                       partitionTerms,
                       neighbourTerms,
                       mergeTerms
                       ) where

import Formula (Formula(..), highestVariableIndex)
import NormalForm
import TruthTable(var, fromTermNumber, Variable, Assignment)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)
import UnboxMaybe

import qualified Data.Vector.Unboxed as V

instance DefaultValue Bool where
    defaultValue = False

newtype QmcTerm = QmcTerm (V.Vector (Maybe Bool))
    deriving (Eq)

-- | The lowest 'Variable' index is shown at the right
instance Show QmcTerm where
    show (QmcTerm vector) = reverse $ map showMaybeBool $ V.toList vector
        where showMaybeBool maybeBool = case maybeBool of
                Just True -> '1'
                Just False -> '0'
                Nothing -> '-'

fromString :: String -> QmcTerm
fromString str = QmcTerm $ V.fromList $ map readMaybeBool $ reverse str
    where readMaybeBool chr = case chr of
                '1' -> Just True
                '0' -> Just False
                _ -> Nothing

formulaToQmcTerms :: Formula -> [QmcTerm]
formulaToQmcTerms formula
    | not (isCanonical formula) = formulaToQmcTerms (ensureCanonical formula)
    | otherwise = map (termToQmcTerm qmcTermLength) terms
    where terms = normalFormChildren formula
          qmcTermLength = highestVariableIndex formula + 1

termToQmcTerm :: Int -> Formula -> QmcTerm
termToQmcTerm qmcTermLength term = QmcTerm (V.generate qmcTermLength $ valueForVariableIndex term)

valueForVariableIndex :: Formula -> Int -> Maybe Bool
valueForVariableIndex term i
    | Atom (var i) `elem` literals = Just True
    | Not (Atom (var i)) `elem` literals = Just False
    | otherwise = Nothing
    where literals = normalFormChildren term

numRelevantLiterals' :: FormType -> QmcTerm -> Int
numRelevantLiterals' formType (QmcTerm vector) = V.foldr countIfRelevant 0 vector
    where countIfRelevant value accum = if valueIsRelevant value then succ accum else accum
          valueIsRelevant value = case (formType, value) of
                (CNFType, Just False) -> True
                (DNFType, Just True) -> True
                _ -> False

groupTerms' :: FormType -> [QmcTerm] -> Map.Map Int [QmcTerm]
groupTerms' formType = Map.fromList . withNumber . group . sortBy (comparing numLiterals)
    where numLiterals = numRelevantLiterals' formType
          group terms = groupBy equalNumRelevantLiterals terms
          equalNumRelevantLiterals t1 t2 = numLiterals t1 == numLiterals t2
          withNumber = map (\terms@(term:_) -> (numLiterals term, terms))

hammingDistance :: (QmcTerm, QmcTerm) -> Int
hammingDistance (QmcTerm v1, QmcTerm v2) = V.sum $ V.zipWith oneIfDifferent v1 v2
    where oneIfDifferent x y = if x /= y then 1 else 0









quineMinterms, quineMaxterms :: Int -> [Formula]
quineMinterms = quineTerms assignmentToMinterm
quineMaxterms = quineTerms assignmentToMaxterm

quineTerms :: (Set.Set Variable -> Assignment -> Formula) -> Int -> [Formula]
quineTerms toTerm numVariables
    | numVariables <= 0 = []
    | otherwise = map (toTerm variables . fromTermNumber) [0..numTerms]
    where numTerms = (2^numVariables)-1
          variables = Set.fromList [minBound..(var $ numVariables-1)]

numRelevantLiterals :: Formula -> Int
numRelevantLiterals formula
    | isConjunctionOfLiterals formula = countPositive $ normalFormChildren formula
    | isDisjunctionOfLiterals formula = countNegative $ normalFormChildren formula
    | otherwise = error $ "Not a conjunction/disjunction of literals: " ++ show formula
    where countPositive = length . filter isPositiveLiteral
          countNegative = length . filter (not . isPositiveLiteral)

groupByRelevantLiterals :: Formula -> Map.Map Int [Formula]
groupByRelevantLiterals formula
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
    where neighbours = neighbourTerms termMap
          neighbourSet = Set.fromList $ foldr (\(a,b) rest -> a:b:rest) [] neighbours
          originalTermSet = Set.fromList . concat . map snd . Map.toList $ termMap
          primeTerms = Set.toList $ Set.difference originalTermSet neighbourSet
          mergedTerms = map mergeTerms neighbours
          

neighbourTerms :: Map.Map Int [Formula] -> [(Formula, Formula)]
neighbourTerms termMap = concatMap allTermPairsForKeyPair neighbourKeyPairs
    where neighbourKeyPairs = neighbourKeys $ Map.keys termMap :: [(Int, Int)]
          allTermPairsForKeyPair (key1,key2) = [(term1,term2) | term1 <- lookupOrEmptyList key1, term2 <- lookupOrEmptyList key2]
          lookupOrEmptyList key = maybe [] id (Map.lookup key termMap)

          

mergeTerms :: (Formula, Formula) -> Formula
mergeTerms termPair = case termPair of
    (Or lits1, Or lits2) -> Or $ merge lits1 lits2
    (And lits1, And lits2) -> And $ merge lits1 lits2
    invalidTerms -> error $ "Invalid term format: " ++ show invalidTerms
    where merge t1 t2 = Set.toAscList $ Set.intersection (Set.fromList t1) (Set.fromList t2)