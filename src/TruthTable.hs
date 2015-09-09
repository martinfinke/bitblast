module TruthTable(TruthTable,
                  emptyTable,
                  getRow,
                  setRow,
                  tableFromList,
                  tableToList,
                  truthTableToString,
                  tableFromString,
                  allFalseTable,
                  allTrueTable,
                  allBoolCombinations,
                  allAssignments,
                  merge,
                  tableVariableSet,
                  trueAndFalse,
                  trim) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List(sort)
import Assignment
import Variable


newtype TruthTable = TruthTable (Map.Map Assignment Bool)
    deriving(Eq, Show, Ord)

truthTableToString :: Set.Set Variable -> TruthTable -> String
truthTableToString varSet (TruthTable rows) =
    unlines . sort . map printRow $ Map.toList rows
    where printRow (assignment,b) = assignmentToString varSet assignment ++ " | " ++ printBool b
          printBool b = if b then "1" else "0"

tableFromString :: Set.Set Variable -> String -> TruthTable
tableFromString varSet str =
    let rows = lines str
        assignments = map (assignmentFromString varSet . assignment) rows
        outputs = map (readOutput . output) rows
        list = zip assignments outputs
    in tableFromList list
    where assignment = takeWhile (/= ' ')
          output = last
          readOutput c = if c == '1' then True else False

emptyTable :: TruthTable
emptyTable = TruthTable Map.empty

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

tableFromList :: [(Assignment, Bool)] -> TruthTable
tableFromList ls = foldr (uncurry setRow) emptyTable ls

tableToList :: TruthTable -> [(Assignment, Bool)]
tableToList (TruthTable assignmentMap) = Map.toAscList assignmentMap

allAssignments :: TruthTable -> [Assignment]
allAssignments (TruthTable assignmentMap) = Map.keys assignmentMap

allTrueTable, allFalseTable :: Set.Set Variable
              -> TruthTable
allFalseTable = allSetTable False
allTrueTable = allSetTable True

allSetTable :: Bool -> Set.Set Variable -> TruthTable
allSetTable b varSet =
    let allAssignments = allBoolCombinations varSet
        allSetToBool = zip allAssignments (repeat b)
    in tableFromList allSetToBool

tableVariableSet :: TruthTable -> Set.Set Variable
tableVariableSet (TruthTable assignmentMap)
    | Map.null assignmentMap = Set.empty
    | otherwise = Set.unions $ map assignedVars $ Map.keys assignmentMap

-- | Partitions a 'TruthTable' into the True and False rows.
trueAndFalse :: TruthTable -> ([Assignment], [Assignment])
trueAndFalse table =
    let list = tableToList table
        trues = map fst . filter snd $ list
        falses = map fst . filter (not . snd) $ list
    in (trues, falses)

-- | Removes columns off a table. This creates duplicate rows, which are merged using ||. This is useful for checking equisatisfiability.
trim :: Set.Set Variable -> TruthTable -> TruthTable
trim varSet table@(TruthTable m)
    | varSet == tableVarSet = table
    | not $ varSet `Set.isSubsetOf` tableVarSet = error "TruthTable.trim: Can't trim a table to a variable set that is not a subset."
    | otherwise = TruthTable $ Map.mapKeysWith (||) reduce m
    where tableVarSet = tableVariableSet table
          reduce = expandOrReduce False varSet

