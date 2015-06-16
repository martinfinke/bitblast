module TruthBased where

import qualified Data.Set as Set
import Variable
import Utils

expand :: Int -> Set.Set Variable -> TruthTable -> [TruthTable]
expand numExtraVars varSet table
    | numExtraVars < 0 = error $ "Number of extra variables can't be < 0: " ++ show numExtraVars
    | numExtraVars == 0 = [table]
    | otherwise = 
    let (branches, newVar) = expandOnOne table varSet
        newVarSet = Set.insert newVar varSet
    in concatMap (expand (numExtraVars-1) newVarSet) branches

expandOnOne :: TruthTable -> Set.Set Variable -> ([TruthTable], Variable)
expandOnOne table varSet =
    let newVar = head $ newVariables varSet
        (trues, falses) = trueAndFalse table
        branches = foldr (\trueAssignment bs -> concatMap (expandOne newVar trueAssignment) bs) [[]] trues
        falseRowsWithExtraVars =
            [(setVar newVar False false, False) | false <- falses] ++
            [(setVar newVar True false, False) | false <- falses]
        withZeros branch = branch ++ falseRowsWithExtraVars
    in (map (tableFromList . withZeros) branches, newVar)

trueAndFalse :: TruthTable -> ([Assignment], [Assignment])
trueAndFalse table =
    let list = tableToList table
        trues = map fst . filter snd $ list
        falses = map fst . filter (not . snd) $ list
    in (trues, falses)

expandOne :: Variable -> Assignment -> [(Assignment, Bool)] -> [[(Assignment, Bool)]]
expandOne newVar trueAssignment branch =
    let extraVarFalse = setVar newVar False trueAssignment
        extraVarTrue = setVar newVar True trueAssignment
    in [
        (extraVarFalse, False) : (extraVarTrue, True) : branch,
        (extraVarFalse, True) : (extraVarTrue, False) : branch,
        (extraVarFalse, True) : (extraVarTrue, True) : branch
        ]
