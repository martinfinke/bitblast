module WorthExtraVariables where

import qualified Data.Set as Set
import Variable
import Utils(combinationsNoMirror)
import Formula
import NormalForm
import Control.Monad(foldM)
import MinimizeFormula
import Data.Maybe
import Debug.Trace(traceShow)

-- | Generates all 2^(2^n) possible 'TruthTable's for a given 'Set.Set' of variables
allPossibleTables :: Set.Set Variable -> [TruthTable]
allPossibleTables varSet =
    let allFalse = allFalseTable varSet
        assignments = allAssignments allFalse
        assignmentSets = concat [combinationsNoMirror len assignments | len <- [0..length assignments]]
        setAssignmentsToTrue assignments = foldr (\assignment table -> setRow assignment True table) allFalse assignments
    in map setAssignmentsToTrue assignmentSets

-- | Finds (maybe) a 'Formula' with a given set of variables, for which it's worth introducing one extra variable. "Worth it" means that the minimized formula is smaller when an extra variable is introduced (compared to no extra variables).
findWorthExtra :: Set.Set Variable -> IO (Maybe Formula)
findWorthExtra varSet =
    let allCnfs = map (getFormula . tableToCnf varSet) $ allPossibleTables varSet
        try maybeResult cnf
            | isJust maybeResult = return maybeResult
            | otherwise = do
                minimizedWithout <- minimizeFormula cnf
                minimizedWithOneExtra <- minimizeTruthBasedWithExtraVarRange (0,1) cnf
                let lits = numLiterals . getStats
                return $ if lits (fst minimizedWithOneExtra) < lits minimizedWithout
                    then Just $ fst minimizedWithOneExtra
                    else Nothing
    in foldM try Nothing allCnfs