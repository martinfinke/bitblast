module TruthBased where

import qualified Data.Set as Set
import Variable
import Formula
import NormalForm
import Debug.Trace(traceShow)

expand :: Int -> Set.Set Variable -> TruthTable -> ([TruthTable], Set.Set Variable)
expand numExtraVars varSet table =
    let result = expand' numExtraVars varSet table
        newVars = Set.fromList $ take numExtraVars $ newVariables varSet
    in (result, newVars)

expand' :: Int -> Set.Set Variable -> TruthTable -> [TruthTable]
expand' numExtraVars varSet table
    | numExtraVars < 0 = error $ "Number of extra variables can't be < 0: " ++ show numExtraVars
    | numExtraVars == 0 = [table]
    | otherwise = 
    let (branches, newVar) = expandOnOne table varSet
        newVarSet = Set.insert newVar varSet
    in concatMap (expand' (numExtraVars-1) newVarSet) branches


expandOnOne :: TruthTable -> Set.Set Variable -> ([TruthTable], Variable)
expandOnOne table varSet =
    let newVar = head $ newVariables varSet
        (trues, falses) = trueAndFalse table
        branches = foldr (\trueAssignment bs -> concatMap (expandOne newVar trueAssignment) bs) [[]] trues
        falseRowsWithExtraVars =
            [(setVar newVar False false, False) | false <- falses] ++
            [(setVar newVar True false, False) | false <- falses]
        withZeros branch = case branch of
            [] -> [(false, False) | false <- falses]
            _ -> branch ++ falseRowsWithExtraVars
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

possibleCnfs :: Int -> Formula -> [(Canonical, [Variable])]
possibleCnfs numExtraVars f =
    let varSet = variableSet f
        (tables, addedVars) = expand numExtraVars varSet (toTruthTable f)
        cnfs = map (tableToCnf (Set.union varSet addedVars)) tables
    in zip cnfs $ repeat (Set.toAscList addedVars)

