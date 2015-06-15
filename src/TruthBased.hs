module TruthBased where

import qualified Data.Set as Set
import Variable
import Utils

expand :: Int -> Set.Set Variable -> TruthTable -> [TruthTable]
expand 0 _ table = [table]
expand numExtraVars varSet table =
    let (branches, newVarSet) = expandOnOne table varSet
    in concatMap (expand (numExtraVars-1) newVarSet) branches

expandOnOne :: TruthTable -> Set.Set Variable -> ([TruthTable], Set.Set Variable)
expandOnOne table varSet =
    let list = tableToList table
        trueAssignments = map fst . filter snd $ list
        falseAssignments = map fst . filter (not . snd) $ list
        (branches,_,addedVars) = foldr (\trueAssignment (bs, newVar:newVars, addedVars) -> (concatMap (expandOne trueAssignment newVar) bs, newVars, Set.insert newVar addedVars)) ([[]], newVariables varSet, Set.empty) trueAssignments
        expandOne trueAssignment newVar branch =
            let extraVarFalse = setVar newVar False trueAssignment
                extraVarTrue = setVar newVar True trueAssignment
            in [
                (extraVarFalse, False) : (extraVarTrue, True) : branch,
                (extraVarFalse, True) : (extraVarTrue, False) : branch,
                (extraVarFalse, True) : (extraVarTrue, True) : branch
                ]
        falseRowsWithExtraVars = concat [[(setVar addedVar False falseAssignment, False), (setVar addedVar True falseAssignment, False)] | falseAssignment <- falseAssignments, addedVar <- Set.toAscList addedVars]
        withZeros branch = branch ++ falseRowsWithExtraVars
    in (map (tableFromList . withZeros) branches, addedVars)