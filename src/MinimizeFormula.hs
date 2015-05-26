module MinimizeFormula where

import TruthTable
import Formula
import NormalForm
import qualified Data.Vector.Unboxed as U
import QmTerm
import Qm

-- | Minimizes an arbitrary 'Formula' to an equivalent CNF/DNF, which consists of the minimal cover of primes.
minimizeFormula :: Formula -> Formula
minimizeFormula = minimizeCanonical . ensureCanonical

-- | Minimizes a 'Canonical' CNF or DNF using the Quine-McCluskey method.
minimizeCanonical :: Canonical -> Formula
minimizeCanonical canonical =
    let terms = canonicalToQmTerms canonical
        qmTermLength = highestVariableIndex (getFormula canonical) + 1
        cnfMode = (getType canonical == CNFType)
        qmFunction = if cnfMode then qmCnf else qm
        minimumCover = qmFunction (map s2b terms) [] []
    in case terms of
        [] -> if cnfMode then And [] else Or []
        _ -> qmTermsToFormula cnfMode qmTermLength minimumCover

-- | Converts a 'Canonical' CNF\/DNF 'Formula' into a list of 'QmTerm's. In a CNF, each clause becomes one 'QmTerm'.
canonicalToQmTerms :: Canonical -> [QmTerm]
canonicalToQmTerms canonical = concatMap (termToQmTerm qmTermLength) terms
    where terms = normalFormChildren formula
          qmTermLength = highestVariableIndex formula + 1
          formula = getFormula canonical


-- | Converts a single min\/maxterm from a DNF/CNF into a 'QmTerm'.
termToQmTerm :: Int -- ^ The length the 'QmcTerm' should have
              -> Formula
              -> [QmTerm]
termToQmTerm qmTermLength term = map QmTerm $ convertDashes (U.generate qmTermLength generator)
    where generator i = valueForVariable term (var i)

-- | Replaces every dash ('Nothing') in the elements of a 'QmTerm' with two instances of the term: One with a 0, and one with a 1. This is done for all possible combinations, so if there are n dashes in a term, the output list has a length of 2^n.
convertDashes :: U.Vector (Maybe Bool) -> [U.Vector (Maybe Bool)]
convertDashes vec
    | U.null vec = [U.empty]
    | otherwise = case U.head vec of
        Nothing -> mapCons True rest ++ mapCons False rest
        Just b -> mapCons b rest
        where rest = convertDashes (U.tail vec)
              mapCons b = map (U.cons $ Just b)

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
    let paddedQmTerms = map (padded originalTermLength) qmTerms
        terms = map (qmTermToTerm cnfMode) paddedQmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms

-- | Converts a 'QmcTerm' back to a minterm (for DNFs) or maxterm (for CNFs). 
qmTermToTerm :: Bool -- ^ 'True' for CNF, 'False' for DNF. If true, the terms are inverted.
              -> QmTerm
              -> Formula
qmTermToTerm cnfMode (QmTerm vector) = op $ U.ifoldr translate [] vector
    where op = if cnfMode then Or else And
          translate i qmcTermElement rest = case qmcTermElement of
                Just b -> invertIfCnf i b : rest
                Nothing -> rest
          invertIfCnf i b = if cnfMode == b then Not (Atom (var i)) else Atom (var i)

-- | Pads a 'QmTerm' with leading zeros ('False' values)
padded :: Int -- ^ The length of the resulting QmTerm
       -> QmTerm
       -> QmTerm
padded len (QmTerm vec) = QmTerm $ zeros U.++ vec
    where zeros = U.replicate (len - U.length vec) (Just False)