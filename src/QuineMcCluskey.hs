module QuineMcCluskey (QmcTerm(..),
                       fromString,
                       formulaToQmcTerms,
                       termToQmcTerm,
                       valueForVariableIndex,
                       numRelevantLiterals,
                       hammingDistance,
                       groupTerms,
                       neighbourKeys,
                       dashesLineUp,
                       mergeTerms,
                       dashWhenDifferent,
                       qmcStep,
                       qmcPrimes,
                       allPairsOfGroups,
                       possibleNeighbours,
                       mergesOrNothing,
                       termsUsedForMerging
                       ) where

import Formula (Formula(..), highestVariableIndex)
import NormalForm
import TruthTable(var, fromTermNumber, Variable, Assignment)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)
import UnboxMaybe
import Data.Maybe(catMaybes, isNothing)
import Debug.Trace(traceShow)

import qualified Data.Vector.Unboxed as V

instance DefaultValue Bool where
    defaultValue = False

type QmcTermElement = Maybe Bool

newtype QmcTerm = QmcTerm (V.Vector QmcTermElement)
    deriving (Eq, Ord)

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

valueForVariableIndex :: Formula -> Int -> QmcTermElement
valueForVariableIndex term i
    | Atom (var i) `elem` literals = Just True
    | Not (Atom (var i)) `elem` literals = Just False
    | otherwise = Nothing
    where literals = normalFormChildren term

numRelevantLiterals :: FormType -> QmcTerm -> Int
numRelevantLiterals formType (QmcTerm vector) = V.foldr countIfRelevant 0 vector
    where countIfRelevant value accum = if valueIsRelevant value then succ accum else accum
          valueIsRelevant value = case (formType, value) of
                (CNFType, Just False) -> True
                (DNFType, Just True) -> True
                _ -> False

groupTerms :: FormType -> [QmcTerm] -> Map.Map Int [QmcTerm]
groupTerms formType = Map.fromList . withNumber . group . sortBy (comparing numLiterals)
    where numLiterals = numRelevantLiterals formType
          group terms = groupBy equalNumRelevantLiterals terms
          equalNumRelevantLiterals t1 t2 = numLiterals t1 == numLiterals t2
          withNumber = map (\terms@(term:_) -> (numLiterals term, terms))

hammingDistance :: (QmcTerm, QmcTerm) -> Int
hammingDistance (QmcTerm v1, QmcTerm v2) = V.sum $ V.zipWith oneIfDifferent v1 v2
    where oneIfDifferent x y = if x /= y then 1 else 0

dashesLineUp :: QmcTerm -> QmcTerm -> Bool
dashesLineUp (QmcTerm v1) (QmcTerm v2) = v1Dashes == v2Dashes
    where [v1Dashes,v2Dashes] = map (V.elemIndices Nothing) [v1,v2]

mergeTerms :: QmcTerm -> QmcTerm -> Maybe QmcTerm
mergeTerms term1@(QmcTerm v1) term2@(QmcTerm v2)
    | len1 /= len2 = error "mergeTerms error: Terms have different lengths."
    | not $ dashesLineUp term1 term2 = Nothing
    | otherwise = if distance == 1 then Just (QmcTerm merge) else Nothing
    where [len1,len2] = map V.length [v1,v2]
          distance = hammingDistance (term1, term2)
          merge = V.zipWith dashWhenDifferent v1 v2

dashWhenDifferent :: QmcTermElement -> QmcTermElement -> QmcTermElement
dashWhenDifferent el1 el2 = case (el1,el2) of
    (Nothing,Nothing) -> Nothing
    (Nothing,_) -> printError
    (_,Nothing) -> printError
    (Just bool1, Just bool2) -> if bool1 /= bool2 then Nothing else Just bool1
    where printError = error $ "dashWhenDifferent error: Dashes don't align. Make sure dashesLineUp was checked before running dashWhenDifferent."

neighbourKeys :: [Int] -> [(Int, Int)]
neighbourKeys ints = [(i,i+1) | i <- ints, (i+1) `elem` ints]

qmcStep :: FormType -> [QmcTerm] -> ([QmcTerm], [QmcTerm])
qmcStep formType terms = (primes, merges)
    where primes = Set.toList $ Set.difference (Set.fromList terms) $ termsUsedForMerging (mergesOrNothing neighbours) neighbours
          merges = catMaybes $ mergesOrNothing neighbours
          groups = groupTerms formType terms
          neighbours = possibleNeighbours groups

allPairsOfGroups :: Map.Map Int [QmcTerm] -> (Int,Int) -> [(QmcTerm, QmcTerm)]
allPairsOfGroups groups (i,j) = [(t1,t2) | t1 <- justOrEmpty i, t2 <- justOrEmpty j]
    where justOrEmpty key = maybe [] id (Map.lookup key groups)

possibleNeighbours :: Map.Map Int [QmcTerm] -> [(QmcTerm,QmcTerm)]
possibleNeighbours groups = concatMap (allPairsOfGroups groups) neighbouringGroups
    where neighbouringGroups = neighbourKeys (Map.keys groups)

mergesOrNothing :: [(QmcTerm,QmcTerm)] -> [Maybe QmcTerm]
mergesOrNothing = map $ uncurry mergeTerms

termsUsedForMerging :: Ord a => [Maybe a] -> [(a,a)] -> Set.Set a
termsUsedForMerging maybeMerges neighbours = Set.fromList $ concat $ zipWith (\mergeOrNothing (t1,t2) -> if isNothing mergeOrNothing then [] else [t1,t2]) maybeMerges neighbours

qmcPrimes :: FormType -> [QmcTerm] -> [QmcTerm]
qmcPrimes _ [] = []
qmcPrimes formType terms =
    let (primes, merges) = qmcStep formType terms
    in primes ++ qmcPrimes formType merges
  