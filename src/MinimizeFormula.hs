module MinimizeFormula where

import TruthTable
import Formula
import NormalForm
import qualified Data.Vector.Unboxed as U
import qualified Data.Bits as B
import Qm

type QmTermEl = Maybe Bool

-- | Minimizes an arbitrary 'Formula' to an equivalent CNF/DNF, which consists of the minimal cover of primes.
minimizeFormula :: Formula -> Formula
minimizeFormula = minimizeCanonical . ensureCanonical

-- | Minimizes a 'Canonical' CNF or DNF using the Quine-McCluskey method.
minimizeCanonical :: Canonical -> Formula
minimizeCanonical canonical =
    let terms = canonicalToBitVectors canonical
        qmTermLength = highestVariableIndex (getFormula canonical) + 1
        cnfMode = (getType canonical == CNFType)
        minimumCover = qm terms [] []
    in case terms of
        [] -> if cnfMode then And [] else Or []
        _ -> qmTermsToFormula cnfMode qmTermLength minimumCover

canonicalToBitVectors :: Canonical -> [BitVector]
canonicalToBitVectors canonical = concatMap (termToBitVectors qmTermLength) terms
    where terms = normalFormChildren formula
          qmTermLength = highestVariableIndex formula + 1
          formula = getFormula canonical

-- TODO: Test
-- | Converts a single min\/maxterm from a DNF/CNF into a 'QmTerm'.
termToBitVectors :: Int -> Formula -> [BitVector]
termToBitVectors qmTermLength term = convertDashes $ foldr setBitForVariable emptyTerm [0..qmTermLength-1]
    where emptyTerm = (QmTerm (0,0))
          setBitForVariable i (QmTerm (bv,mask)) = QmTerm $ case valueForVariable term (var i) of
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
                 -> Int -- ^ The original number of variables (i.e. the highest index + 1)
                 -> [QmTerm]
                 -> Formula
qmTermsToFormula cnfMode originalTermLength qmTerms =
    let terms = map (qmTermToTerm cnfMode originalTermLength) qmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms

-- | Converts a 'QmcTerm' back to a minterm (for DNFs) or maxterm (for CNFs). 
qmTermToTerm :: Bool -- ^ 'True' for CNF, 'False' for DNF. If true, the terms are inverted.
             -> Int -- ^ The original number of variables
             -> QmTerm
             -> Formula
qmTermToTerm cnfMode originalTermLength (QmTerm (term,mask)) = op $ foldr translate [] [0..originalTermLength-1]
    where op = if cnfMode then Or else And
          translate i restLiterals
                | B.testBit mask i = restLiterals
                | otherwise = invertIfCnf i (B.testBit term i) : restLiterals
          invertIfCnf i b = if cnfMode == b then Not (Atom (var i)) else Atom (var i)