module Variable(
                Variable,
                initial,
                eval,
                generateVars,
                var,
                Assignment,
                emptyAssignment,
                allTrue,
                allFalse,
                assignmentFromList,
                assignmentFromString,
                assignmentToString,
                expandOrReduce,
                getVar,
                setVar,
                TruthTable,
                emptyTable,
                getRow,
                setRow,
                tableFromList,
                truthTableToString,
                allFalseTable,
                allTrueTable,
                allBoolCombinations
                ) where

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(forM)
import Text.Printf(printf)
import Data.List(sort)

data VarMem = VarMem {currentVarIndex :: Int, positionMapping :: [Variable]}
type VarState = State.State VarMem
type VarStateTransformer = State.StateT VarMem
newtype Variable = Variable Int
    deriving(Eq, Ord)

-- TODO: Show shouldn't be there. Only possible in a VarState
instance Show Variable where
    show (Variable i) = show i

-- TODO: Enum shouldn't be there.
instance Enum Variable where
    toEnum = Variable
    fromEnum (Variable i) = i

initial :: VarMem
initial = VarMem {currentVarIndex=0, positionMapping=[]}

eval :: VarMem -> VarState a -> a
eval = flip State.evalState

generateVars :: Int -- ^ How many 'Variable's to create
             -> ([Variable], [Variable]) -- ^ A list of 'Variable's, and the positionMapping.
generateVars numvars = eval initial $ do
    variables <- forM [0..numvars-1] $ \i -> var ('x' : show i)
    posMapping <- fmap positionMapping State.get
    return (variables, posMapping)

var :: Monad m => String -> VarStateTransformer m Variable
var varName = do
    varIndex <- fmap currentVarIndex State.get
    let newVar = Variable varIndex
    State.modify (addVariable newVar)
    return newVar

addVariable :: Variable -> VarMem -> VarMem
addVariable variable mem@(VarMem {currentVarIndex=idx, positionMapping=oldMapping}) =
    mem{currentVarIndex=succ idx,positionMapping=variable : oldMapping}

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

assignmentFromString :: [Variable] -> String -> Assignment
assignmentFromString posMapping string
    | length posMapping /= length string = error $ printf "The string %s has the wrong length (%d) for the positionMapping of length %d." string (length string) (length posMapping)
    | otherwise = foldr parse emptyAssignment $ zip string posMapping
        where parse (c,variable) assignment =
                if c == '0'
                    then setVar variable False assignment
                    else setVar variable True assignment

assignmentToString :: [Variable] -> Assignment -> String
assignmentToString posMapping (Assignment intMap) =
    map printVar posMapping
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

newtype TruthTable = TruthTable (Map.Map Assignment Bool)
    deriving(Eq, Show)

truthTableToString :: [Variable] -> TruthTable -> String
truthTableToString posMapping (TruthTable rows) =
    unlines . sort . map printRow $ Map.toList rows
    where printRow (assignment,b) = assignmentToString posMapping assignment ++ " | " ++ printBool b
          printBool b = if b then "1" else "0"

emptyTable :: TruthTable
emptyTable = TruthTable Map.empty

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

tableFromList :: [(Assignment, Bool)] -> TruthTable
tableFromList ls = foldr (uncurry setRow) emptyTable ls

allTrueTable, allFalseTable :: [Variable] -- ^ positionMapping
              -> TruthTable
allFalseTable = allSetTable False
allTrueTable = allSetTable True

allSetTable :: Bool -> [Variable] -> TruthTable
allSetTable b posMapping =
    let allAssignments = allBoolCombinations (Set.fromList posMapping)
        allSetToBool = zip allAssignments (repeat b)
    in tableFromList allSetToBool



-- | All possible 'Assignment's for a 'Set.Set' of 'Variable's.
allBoolCombinations :: Set.Set Variable -> [Assignment]
allBoolCombinations variables = allBoolCombinations' (allFalse variables) variables

allBoolCombinations' allFalse' variables
    | Set.null variables = [allFalse']
    | otherwise = rest ++ map (setVar variable True) rest
    where variable = Set.findMax variables
          rest = allBoolCombinations' allFalse' (Set.delete variable variables)
