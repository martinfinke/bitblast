module TruthBased where

import qualified Data.Set as Set
import Variable
import Formula
import NormalForm
import Utils
import Debug.Trace(traceShow)

-- TODO: remove this and only use the one below
expand :: Int -> Set.Set Variable -> TruthTable -> ([TruthTable], Set.Set Variable)
expand = expand'

expand' :: Int -> Set.Set Variable -> TruthTable -> ([TruthTable], Set.Set Variable)
expand' numExtraVars varSet table
    | numExtraVars < 0 = error $ "Number of extra variables can't be < 0: " ++ show numExtraVars
    | numExtraVars == 0 = ([table], varSet)
    | otherwise = 
    let (branches, newVar) = expandOnOne table varSet
        newVarSet = Set.insert newVar varSet
        rest = map (expand' (numExtraVars-1) newVarSet) branches
    in (concatMap fst rest, snd $ head rest)


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

-- | Partitions a 'TruthTable' into the True and False rows.
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

-- | All possible 'Canonical' CNFs for a 'Formula' and a given number of extra variables.
possibleCnfs :: Int -> Formula -> [(Canonical, [Variable])]
possibleCnfs numExtraVars f =
    let varSet = variableSet f
        table = toTruthTable f
    in possibleCnfsFromTable numExtraVars varSet table

possibleCnfsFromTable :: Int -> Set.Set Variable -> TruthTable -> [(Canonical, [Variable])]
possibleCnfsFromTable numExtraVars varSet table =
    let (tables, addedVars) = expand'' numExtraVars table
        newVarSet = Set.union varSet addedVars
        cnfs = map (tableToCnf newVarSet) tables
    in zip cnfs $ repeat (Set.toAscList addedVars)


combinations :: Set.Set Variable -> Set.Set Variable -> [Assignment] -> [TruthTable]
combinations oldVarSet extraVars trueAssignments =
    let allForExtraVars = allBoolCombinations extraVars :: [Assignment]
        -- for each trueAssignment, all possible expansions:
        allExpansions = [[merge original extra | extra <- allForExtraVars] | original <- trueAssignments] :: [[Assignment]]
        -- for each trueAssignment, all possible non-empty selections of expansions:
        nonEmptySelections = map atLeastOne allExpansions :: [[[Assignment]]]
        combos = map concat (oneFromEachSublist nonEmptySelections) :: [[Assignment]]
        onlyTrueFor assignments = foldr (flip setRow True) (allFalseTable $ Set.union oldVarSet extraVars) assignments
        tables = map onlyTrueFor combos
    in tables
        
expand'' :: Int -> TruthTable -> ([TruthTable], Set.Set Variable)
expand'' numExtraVars table =
    let oldVarSet = tableVariableSet table
        extraVars = Set.fromList $ take numExtraVars $ newVariables oldVarSet
        (trues,_) = trueAndFalse table
        tables = combinations oldVarSet extraVars trues
    in (tables, extraVars)

atLeastOne :: Ord a => [a] -> [[a]]
atLeastOne list = concat [combinationsNoMirror len list | len <- [1..length list]]