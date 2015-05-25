module MinimizeFormula where

import TruthTable
import Formula
import NormalForm
import qualified Data.Vector.Unboxed as U
import QmTerm
import Qm

formulaToPrimesFormula :: Formula -> Formula
formulaToPrimesFormula = minimizeCanonical . ensureCanonical

minimizeCanonical :: Canonical -> Formula
minimizeCanonical canonical =
    let terms = canonicalToQmTerms canonical
        cnfMode = (getType canonical == CNFType)
        qmFunction = if cnfMode then qmCnf else qm
        minimumCover = qmFunction (map s2b terms) [] []
    in case terms of
        [] -> if cnfMode then And [] else Or []
        _ -> qmTermsToFormula cnfMode minimumCover

-- | Converts a 'Canonical' CNF\/DNF 'Formula' into a list of 'QmTerm's. In a CNF, each clause becomes one 'QmTerm'.
canonicalToQmTerms :: Canonical -> [QmTerm]
canonicalToQmTerms canonical = map (termToQmTerm qmTermLength) terms
    where terms = normalFormChildren formula
          qmTermLength = highestVariableIndex formula + 1
          formula = getFormula canonical


-- | Converts a single min\/maxterm from a DNF/CNF into a 'QmTerm'.
termToQmTerm :: Int -- ^ The length the 'QmcTerm' should have
              -> Formula
              -> QmTerm
termToQmTerm qmTermLength term = QmTerm (U.generate qmTermLength $ valueForVariable term . var)

-- | Converts a 'QmcTerm' back to a minterm (for DNFs) or maxterm (for CNFs). 
qmTermToTerm :: Bool -- ^ 'True' for CNF, 'False' for DNF
              -> QmTerm
              -> Formula
qmTermToTerm cnfMode (QmTerm vector) = op $ U.ifoldr translate [] vector
    where op = if cnfMode then Or else And
          translate i qmcTermElement rest = case qmcTermElement of
                Just True -> Atom (var i) : rest
                Just False -> Not (Atom (var i)) : rest
                Nothing -> rest

-- | Extracts the value of a 'Variable' in a term. Used to convert terms to 'QmcTerm's.
valueForVariable :: Formula -> Variable -> QmTermEl
valueForVariable term variable
    | Atom variable `elem` literals = Just True
    | Not (Atom variable) `elem` literals = Just False
    | otherwise = Nothing
    where literals = normalFormChildren term

qmTermsToFormula :: Bool -> [QmTerm] -> Formula
qmTermsToFormula cnfMode qmTerms =
    let terms = map (qmTermToTerm cnfMode) qmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms 