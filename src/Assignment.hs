module Assignment(
                Assignment,
                emptyAssignment,
                allTrue,
                allFalse,
                assignmentFromList,
                assignmentToList,
                assignmentFromString,
                assignmentToString,
                expandOrReduce,
                getVar,
                setVar,
                assignedVars,
                newVariables,
                allBoolCombinations,
                merge) where

import Variable

import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Text.Printf(printf)
import Data.List(sortBy)
import Data.Ord(comparing)

newtype Assignment = Assignment (IntMap.IntMap Bool)
    deriving(Eq, Show, Ord)

emptyAssignment :: Assignment
emptyAssignment = Assignment IntMap.empty

allTrue, allFalse :: Set.Set Variable -> Assignment
(allTrue, allFalse) = (forAllVars True, forAllVars False)
    where forAllVars b = Set.foldr (flip setVar b) emptyAssignment

assignmentFromList :: [(Variable, Bool)] -> Assignment
assignmentFromList ls =
    let ls' = map (\(Variable i, b) -> (i,b)) ls
    in Assignment $ IntMap.fromList ls'

assignmentToList :: Assignment -> [(Variable, Bool)]
assignmentToList (Assignment intMap) = sortBy (comparing fst) . map (\(i,b) -> (var i, b)) . IntMap.toList $ intMap

assignmentFromString :: Set.Set Variable -> String -> Assignment
assignmentFromString varSet string
    | Set.size varSet /= length string = error $ printf "The string %s has the wrong length (%d) for the variable set of size %d." string (length string) (Set.size varSet)
    | otherwise = foldr parse emptyAssignment $ zip (reverse string) (Set.toAscList varSet)
        where parse (c,variable) assignment =
                if c == '0'
                    then setVar variable False assignment
                    else setVar variable True assignment

assignmentToString :: Set.Set Variable -> Assignment -> String
assignmentToString varSet (Assignment intMap) = reverse $ map printVar $ Set.toAscList varSet
    where printVar (Variable i) = case IntMap.lookup i intMap of
            Nothing -> '-'
            Just True -> '1'
            Just False -> '0'

getVar :: Variable -> Assignment -> Maybe Bool
getVar (Variable i) (Assignment intMap) = IntMap.lookup i intMap

setVar :: Variable -> Bool -> Assignment -> Assignment
setVar (Variable i) b (Assignment intMap) = Assignment $ IntMap.insert i b intMap

assignedVars :: Assignment -> Set.Set Variable
assignedVars (Assignment intMap) = Set.fromList $ map Variable (IntMap.keys intMap)

expandOrReduce :: Bool -> Set.Set Variable -> Assignment -> Assignment
expandOrReduce b variableSet assignment@(Assignment intMap) =
    let alreadyAssigned = assignedVars assignment
        toAssign = Set.difference variableSet alreadyAssigned
        toRemove = Set.difference alreadyAssigned variableSet
        removed = IntMap.filterWithKey (\k _ -> (Variable k) `Set.notMember` toRemove) intMap
        added = Set.foldr (\(Variable i) intMap' -> IntMap.insert i b intMap') removed toAssign
    in Assignment added

-- | All possible 'Assignment's for a 'Set.Set' of 'Variable's.
allBoolCombinations :: Set.Set Variable -> [Assignment]
allBoolCombinations variables = allBoolCombinations' (allFalse variables) variables

allBoolCombinations' allFalse' variables
    | Set.null variables = [allFalse']
    | otherwise = rest ++ map (setVar variable True) rest
    where variable = Set.findMax variables
          rest = allBoolCombinations' allFalse' (Set.delete variable variables)

merge :: Assignment -> Assignment -> Assignment
merge (Assignment lhs) (Assignment rhs) = Assignment $ IntMap.union lhs rhs
