module MinimizeFormula where

import Variable
import Formula
import NormalForm
import qualified Data.Vector.Unboxed as U
import qualified Data.Bits as B
import Qm

type QmTermEl = Maybe Bool

-- | Minimizes an arbitrary 'Formula' to an equivalent CNF/DNF, which consists of the minimal cover of primes.
minimizeFormula :: [Variable] -> Formula -> Formula
minimizeFormula positionMapping formula =
    let canonical = ensureCanonical formula
    in minimizeCanonical positionMapping canonical

-- | Minimizes a 'Canonical' CNF or DNF using the Quine-McCluskey method.
minimizeCanonical :: [Variable] -> Canonical -> Formula
minimizeCanonical positionMapping canonical =
    let terms = canonicalToBitVectors positionMapping canonical
        cnfMode = (getType canonical == CNFType)
        minimumCover = qm terms [] []
    in case terms of
        [] -> if cnfMode then And [] else Or []
        _ -> qmTermsToFormula cnfMode positionMapping minimumCover

canonicalToBitVectors :: [Variable] -> Canonical -> [BitVector]
canonicalToBitVectors positionMapping canonical = concatMap (termToBitVectors positionMapping) terms
    where terms = normalFormChildren formula
          formula = getFormula canonical

-- TODO: Test
termToBitVectors :: [Variable] -> Formula -> [BitVector]
termToBitVectors positionMapping term = convertDashes $ foldr setBitForVariable emptyTerm variablesWithPositions
    where emptyTerm = (QmTerm (0,0))
          variablesWithPositions = zip [0..] positionMapping
          setBitForVariable (i,variable) (QmTerm (bv,mask)) = QmTerm $ case valueForVariable term variable of
            Nothing -> (bv, B.setBit mask i)
            Just bool -> (if bool then B.setBit bv i else B.clearBit bv i, mask)

-- | Replaces every dash ('Nothing') in the elements of a 'QmTerm' with two instances of the term: One with a 0, and one with a 1. This is done for all possible combinations, so if there are n dashes in a term, the output list has a length of 2^n.
-- TODO: Test
convertDashes :: QmTerm -> [BitVector]
convertDashes qmTerm = foldr forEachBit [term] [0..B.finiteBitSize term - 1]
    where term = getTerm qmTerm
          mask = getMask qmTerm
          forEachBit pos bvs
                | B.testBit mask pos = map (flip B.setBit pos) bvs ++ map (flip B.clearBit pos) bvs
                | otherwise = bvs

-- | Extracts the value of a 'Variable' in a term. Used to convert terms to 'QmcTerm's. If the input 'Formula' is a CNF, the values are inverted (i.e. a positive literal results in a 0).
valueForVariable :: Formula -> Variable -> QmTermEl
valueForVariable term variable
    | Atom variable `elem` literals = Just $ if cnfMode then False else True
    | Not (Atom variable) `elem` literals = Just $ if cnfMode then True else False
    | otherwise = Nothing
    where literals = normalFormChildren term
          cnfMode = case term of
                Or _ -> True
                And _ -> False

-- | Converts a list of 'QmTerm's back to a 'Formula'
qmTermsToFormula :: Bool -- ^ Whether the 'Formula' was a CNF. If so, the terms will be inverted.
                 -> [Variable] -- ^ The Position Mapping
                 -> [QmTerm]
                 -> Formula
qmTermsToFormula cnfMode positionMapping qmTerms =
    let terms = map (qmTermToTerm cnfMode positionMapping) qmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms

-- | Converts a 'QmcTerm' back to a minterm (for DNFs) or maxterm (for CNFs). 
qmTermToTerm :: Bool -- ^ 'True' for CNF, 'False' for DNF. If true, the terms are inverted.
             -> [Variable] -- ^ The Position Mapping
             -> QmTerm
             -> Formula
qmTermToTerm cnfMode positionMapping (QmTerm (term,mask)) = op $ foldr translate [] variablesWithPositions
    where variablesWithPositions = zip [0..] positionMapping
          op = if cnfMode then Or else And
          translate (i,variable) restLiterals
                | B.testBit mask i = restLiterals
                | otherwise = invertIfCnf variable (B.testBit term i) : restLiterals
          invertIfCnf v b = if cnfMode == b then Not (Atom v) else Atom v