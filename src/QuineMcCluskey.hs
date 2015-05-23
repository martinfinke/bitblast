{-|
The Quine-McCluskey algorithm for boolean function optimization.
-}
module QuineMcCluskey (formulaToPrimesFormula,
                       qmcPrimes,
                       QmcTerm,
                       fromString,
                       QmcTermElement,
                       canonicalToQmcTerms,
                       termToQmcTerm,
                       qmcTermToTerm,
                       valueForVariable,
                       groupTerms,
                       GroupedTerms,
                       numRelevantLiterals,
                       neighbourKeys,
                       hammingDistance,
                       dashesLineUp,
                       mergeTerms,
                       dashWhenDifferent,
                       qmcStep,
                       allPairsOfGroups,
                       possibleNeighbours,
                       mergesOrNothing,
                       termsUsedForMerging,
                       isCoveredBy,
                       dropElement,
                       emptyState,
                       removeRow,
                       removeColumn,
                       essentialColumns
                       ) where

import Formula (Formula(..), highestVariableIndex)
import NormalForm
import TruthTable(var, Variable)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)
import UnboxMaybe
import Data.Maybe(catMaybes, isNothing)
import MatrixUtils
import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix as M
import qualified Data.Vector as Vec


-- | Converts any 'Formula' into a CNF\/DNF consisting of all prime terms. If the input is a 'Canonical' DNF, the output is a DNF. Otherwise, the output is a CNF.
formulaToPrimesFormula :: Formula -> Formula
formulaToPrimesFormula formula =
    let canonical = ensureCanonical formula
        formType = getType canonical
        qmcTerms = canonicalToQmcTerms canonical
        primes = qmcPrimes formType qmcTerms
        translatedTerms = map (qmcTermToTerm formType) primes
        rootOp = if formType == CNFType then And else Or
    in rootOp translatedTerms

-- | Converts a list of 'QmcTerm's into the list of its prime terms.
qmcPrimes :: FormType -> [QmcTerm] -> [QmcTerm]
qmcPrimes _ [] = [] -- No merges were possible, so now it would be the time to find the minimal cover.
qmcPrimes formType terms =
    let (primes, merges) = qmcStep formType terms
    in primes ++ qmcPrimes formType merges
  
-- | The basic term type used in the algorithm. It's a sequence of either a 'Bool' value, or a dash (indicating don't-care).
newtype QmcTerm = QmcTerm (V.Vector QmcTermElement)
    deriving (Eq, Ord)

-- | Shows a QmcTerm as a sequence of 0 ('False'), 1 ('True') or - (dash, indicating a don't-care value). The lowest 'Variable' index is shown at the right.
instance Show QmcTerm where
    show (QmcTerm vector) = reverse $ map showMaybeBool $ V.toList vector
        where showMaybeBool maybeBool = case maybeBool of
                Just True -> '1'
                Just False -> '0'
                Nothing -> '-'

-- | Converts a 'String' of 0, 1 or - (dash) into a 'QmcTerm'. The 'String' is parsed right-to-left, so the lowest 'Variable' index is at the end of the 'String'. This is consistent with the 'Show' instance.
fromString :: String -> QmcTerm
fromString str = QmcTerm $ V.fromList $ map readMaybeBool $ reverse str
    where readMaybeBool chr = case chr of
                '1' -> Just True
                '0' -> Just False
                _ -> Nothing

-- | One position in a 'QmcTerm'. 'Nothing' indicates a don't-care at that position.
type QmcTermElement = Maybe Bool

-- | Converts a 'Canonical' CNF\/DNF 'Formula' into a list of 'QmcTerm's. In a CNF, each clause becomes one 'QmcTerm'.
canonicalToQmcTerms :: Canonical -> [QmcTerm]
canonicalToQmcTerms canonical = map (termToQmcTerm qmcTermLength) terms
    where terms = normalFormChildren formula
          qmcTermLength = highestVariableIndex formula + 1
          formula = getFormula canonical

-- | Converts a single min\/maxterm from a DNF/CNF into a 'QmcTerm'.
termToQmcTerm :: Int -- ^ The length the 'QmcTerm' should have
              -> Formula
              -> QmcTerm
termToQmcTerm qmcTermLength term = QmcTerm (V.generate qmcTermLength $ valueForVariable term . var)

-- | Converts a 'QmcTerm' back to a minterm (for DNFs) or maxterm (for CNFs). 
qmcTermToTerm :: FormType -- ^ 'DNFType' to get a minterm, 'CNFType' to get a maxterm
              -> QmcTerm -> Formula
qmcTermToTerm formType (QmcTerm vector) = op $ V.ifoldr translate [] vector
    where op = if formType == CNFType then Or else And
          translate i qmcTermElement rest = case qmcTermElement of
                Just True -> Atom (var i) : rest
                Just False -> Not (Atom (var i)) : rest
                Nothing -> rest

-- | Extracts the value of a 'Variable' in a term. Used to convert terms to 'QmcTerm's.
valueForVariable :: Formula -> Variable -> QmcTermElement
valueForVariable term variable
    | Atom variable `elem` literals = Just True
    | Not (Atom variable) `elem` literals = Just False
    | otherwise = Nothing
    where literals = normalFormChildren term

-- | Groups 'QmcTerm's by the number of relevant literals in them. For CNFs, the 'False' literals (0) are relevant; for DNFs the 'True' literals (1).
groupTerms :: FormType -> [QmcTerm] -> GroupedTerms
groupTerms formType = IntMap.fromList . withNumber . group . sortBy (comparing numLiterals)
    where numLiterals = numRelevantLiterals formType
          group terms = groupBy equalNumRelevantLiterals terms
          equalNumRelevantLiterals t1 t2 = numLiterals t1 == numLiterals t2
          withNumber = map (\terms@(term:_) -> (numLiterals term, terms))

-- | Grouped 'QmcTerm's are stored in an 'Data.IntMap.Lazy.IntMap'. The keys are the numbers of literals.
type GroupedTerms = IntMap.IntMap [QmcTerm]

-- | Counts how many relevant literals there are in a 'QmcTerm'. For CNFs, the 'False' literals (0) are relevant; for DNFs the 'True' literals (1).
numRelevantLiterals :: FormType -> QmcTerm -> Int
numRelevantLiterals formType (QmcTerm vector) = V.foldr countIfRelevant 0 vector
    where countIfRelevant value accum = if valueIsRelevant value then succ accum else accum
          valueIsRelevant value = case (formType, value) of
                (CNFType, Just False) -> True
                (DNFType, Just True) -> True
                _ -> False

-- | For a list of numbers, yields all tuples of two numbers that are adjacent to each other.
neighbourKeys :: [Int] -> [(Int, Int)]
neighbourKeys ints = [(i,i+1) | i <- ints, (i+1) `elem` ints]

-- | The number of 'QmcTermElement's that differ between two 'QmcTerm's.
hammingDistance :: (QmcTerm, QmcTerm) -> Int
hammingDistance (QmcTerm v1, QmcTerm v2) = V.sum $ V.zipWith oneIfDifferent v1 v2
    where oneIfDifferent x y = if x /= y then 1 else 0

-- | 'True' if the don't-cares (dashes) of two terms line up, i.e. the set of their indices is the same for both terms.
dashesLineUp :: QmcTerm -> QmcTerm -> Bool
dashesLineUp (QmcTerm v1) (QmcTerm v2) = v1Dashes == v2Dashes
    where [v1Dashes,v2Dashes] = map (V.elemIndices Nothing) [v1,v2]

-- | Tries to merge two 'QmcTerm's. This is allowed iff (1) their 'dashesLineUp', and (2) if their 'hammingDistance' is exactly 1. If it's allowed, the terms are merged using the 'dashWhenDifferent' function. Returns 'Nothing' if the two terms can't be merged.
mergeTerms :: QmcTerm -> QmcTerm -> Maybe QmcTerm
mergeTerms term1@(QmcTerm v1) term2@(QmcTerm v2)
    | len1 /= len2 = error "mergeTerms error: Terms have different lengths."
    | not $ dashesLineUp term1 term2 = Nothing
    | otherwise = if distance == 1 then Just (QmcTerm merge) else Nothing
    where [len1,len2] = map V.length [v1,v2]
          distance = hammingDistance (term1, term2)
          merge = V.zipWith dashWhenDifferent v1 v2

-- | Used to merge two terms. If the 'QmcTermElement's are the same, the result is the first one. If they are different, the result is a don't-care (dash).
dashWhenDifferent :: QmcTermElement -> QmcTermElement -> QmcTermElement
dashWhenDifferent el1 el2 = case (el1,el2) of
    (Nothing,Nothing) -> Nothing
    (Nothing,_) -> printError
    (_,Nothing) -> printError
    (Just bool1, Just bool2) -> if bool1 /= bool2 then Nothing else Just bool1
    where printError = error $ "dashWhenDifferent error: Dashes don't align. Make sure dashesLineUp was checked before running dashWhenDifferent."

-- | Performs one step in the Quine-McCluskey algorithm, i.e. takes a list of terms and determines the primes/merges for that one step.
qmcStep :: FormType -> [QmcTerm] -> ([QmcTerm], [QmcTerm])
qmcStep formType terms = (primes, merges)
    where primes = Set.toList $ Set.difference (Set.fromList terms) $ termsUsedForMerging (mergesOrNothing neighbours) neighbours
          merges = catMaybes $ mergesOrNothing neighbours
          groups = groupTerms formType terms
          neighbours = possibleNeighbours groups

allPairsOfGroups :: GroupedTerms -> (Int,Int) -> [(QmcTerm, QmcTerm)]
allPairsOfGroups groups (i,j) = [(t1,t2) | t1 <- justOrEmpty i, t2 <- justOrEmpty j]
    where justOrEmpty key = maybe [] id (IntMap.lookup key groups)

possibleNeighbours :: GroupedTerms -> [(QmcTerm,QmcTerm)]
possibleNeighbours groups = concatMap (allPairsOfGroups groups) neighbouringGroups
    where neighbouringGroups = neighbourKeys (IntMap.keys groups)

mergesOrNothing :: [(QmcTerm,QmcTerm)] -> [Maybe QmcTerm]
mergesOrNothing = map $ uncurry mergeTerms

termsUsedForMerging :: Ord a => [Maybe a] -> [(a,a)] -> Set.Set a
termsUsedForMerging maybeMerges neighbours = Set.fromList $ concat $ zipWith (\mergeOrNothing (t1,t2) -> if isNothing mergeOrNothing then [] else [t1,t2]) maybeMerges neighbours







type MinimizationState = ([QmcTerm], [QmcTerm], M.Matrix Bool)

emptyState :: [QmcTerm] -> [QmcTerm] -> MinimizationState
emptyState terms primes = (terms, primes, matrix)
    where matrix = M.matrix (length terms) (length primes) termIsCovered
          termIsCovered (i,j) = (terms!!(i-1)) `isCoveredBy` (primes!!(j-1))

isCoveredBy :: QmcTerm -> QmcTerm -> Bool
isCoveredBy (QmcTerm termVec) (QmcTerm primeVec) = V.all id $ V.zipWith isCovered termVec primeVec
    where isCovered termEl primeEl = case primeEl of
            Nothing -> True
            _ -> termEl == primeEl

removeRow :: Int -> MinimizationState -> MinimizationState
removeRow rowIndex (terms, primes, matrix) =
    (dropElement rowIndex terms, primes, dropMatrixRow matrix rowIndex)

removeColumn :: Int -> MinimizationState -> MinimizationState
removeColumn columnIndex (terms, primes, matrix) =
    (terms, dropElement columnIndex primes, dropMatrixColumn matrix columnIndex)

dropElement :: Int -> [a] -> [a]
dropElement i list = before ++ tail remainder
    where (before, remainder) = splitAt i list

essentialColumns :: M.Matrix Bool -> [Int]
essentialColumns matrix = catMaybes $ map checkRow [1..M.nrows matrix]
    where checkRow i = isCoveredByOnlyOneColumn (M.getRow i matrix)

isCoveredByOnlyOneColumn :: Vec.Vector Bool -> Maybe Int
isCoveredByOnlyOneColumn row
    | numMarks == 1 = Vec.elemIndex True row
    | otherwise = Nothing
    where numMarks = Vec.foldr (\el rest -> if el then rest + 1 else rest) 0 row