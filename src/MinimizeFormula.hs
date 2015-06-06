module MinimizeFormula where

import Variable
import Formula
import NormalForm
import qualified Data.Set as Set
import qualified Data.Bits as B
import QmcTypes

canonicalToBitVectors :: Set.Set Variable -> Canonical -> [BitVector]
canonicalToBitVectors varSet canonical = concatMap (convertDashes . packTerm varSet) terms
    where terms = normalFormChildren formula
          formula = getFormula canonical

-- | Replaces every dash ('Nothing') in the elements of a 'QmTerm' with two instances of the term: One with a 0, and one with a 1. This is done for all possible combinations, so if there are n dashes in a term, the output list has a length of 2^n.
convertDashes :: QmTerm -> [BitVector]
convertDashes qmTerm = foldr forEachBit [term] [0..B.finiteBitSize term - 1]
    where term = getTerm qmTerm
          mask = getMask qmTerm
          forEachBit pos bvs
                | B.testBit mask pos = map (flip B.setBit pos) bvs ++ map (flip B.clearBit pos) bvs
                | otherwise = bvs

-- | Extracts the value of a 'Variable' in a term. Used to convert terms to 'QmcTerm's. If the input 'Formula' is a CNF, the values are inverted (i.e. a positive literal results in a 0).
valueForVariable :: Formula -> Variable -> Maybe Bool
valueForVariable term variable
    | Atom variable `elem` literals = Just $ if cnfMode then False else True
    | Not (Atom variable) `elem` literals = Just $ if cnfMode then True else False
    | otherwise = Nothing
    where literals = normalFormChildren term
          cnfMode = case term of
                Or _ -> True
                And _ -> False

-- | Converts a list of 'QmTerm's back to a 'Formula'
qmTermsToFormula :: Set.Set Variable -- ^ The variable set of the formula
                 -> Bool -- ^ Whether the 'Formula' was a CNF. If so, the terms will be inverted.
                 -> [QmTerm]
                 -> Formula
qmTermsToFormula varSet cnfMode qmTerms =
    let terms = map (unpackTerm cnfMode varSet) qmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms

packTerm :: Set.Set Variable -> Formula -> QmTerm
packTerm varSet term =
    let emptyQmTerm = (QmTerm (0,0))
        appearsInTerm = flip Set.member varSet
        selection = filter appearsInTerm (Set.toAscList varSet)
        variablesWithPositions = zip [0..] selection
        setBitForVariable (i,variable) (QmTerm (bv,mask)) = QmTerm $ case valueForVariable term variable of
            Nothing -> (bv, B.setBit mask i)
            Just bool -> (if bool then B.setBit bv i else B.clearBit bv i, mask)
    in foldr setBitForVariable emptyQmTerm variablesWithPositions

unpackTerm :: Bool -> Set.Set Variable -> QmTerm -> Formula
unpackTerm cnfMode varSet (QmTerm (term,mask)) =
    let appearsInTerm = flip Set.member varSet
        selection = filter appearsInTerm (Set.toAscList varSet)
        variablesWithPositions = zip [0..] selection
        op = if cnfMode then Or else And
        invertIfCnf v b = if cnfMode == b then Not (Atom v) else Atom v
        setVariableForBit (i,variable) restLiterals
              | B.testBit mask i = restLiterals
              | otherwise = invertIfCnf variable (B.testBit term i) : restLiterals
    in op $ foldr setVariableForBit [] variablesWithPositions