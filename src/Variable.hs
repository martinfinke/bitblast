module Variable(
                Variable,
                initial,
                eval,
                makeVars,
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

data VarMem = VarMem {currentVarIndex :: Int}
type VarState = State.State VarMem
type VarStateTransformer = State.StateT VarMem
newtype Variable = Variable Int
    deriving(Eq, Ord)

instance Show Variable where
    show (Variable i) = show i

instance Enum Variable where
    fromEnum (Variable i) = i
    toEnum = Variable

initial :: VarMem
initial = VarMem {currentVarIndex=0}

eval :: VarMem -> VarState a -> a
eval = flip State.evalState

makeVars :: Int -- ^ How many 'Variable's to create
             -> [Variable]
makeVars numvars = eval initial $ do
    forM [0..numvars-1] $ const var

var :: Monad m => VarStateTransformer m Variable
var = do
    varIndex <- fmap currentVarIndex State.get
    let newVar = Variable varIndex
    State.modify addVariable
    return newVar

addVariable :: VarMem -> VarMem
addVariable mem@(VarMem {currentVarIndex=idx}) =
    mem{currentVarIndex=succ idx}

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

newtype TruthTable = TruthTable (Map.Map Assignment Bool)
    deriving(Eq, Show)

truthTableToString :: Set.Set Variable -> TruthTable -> String
truthTableToString varSet (TruthTable rows) =
    unlines . sort . map printRow $ Map.toList rows
    where printRow (assignment,b) = assignmentToString varSet assignment ++ " | " ++ printBool b
          printBool b = if b then "1" else "0"

emptyTable :: TruthTable
emptyTable = TruthTable Map.empty

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

tableFromList :: [(Assignment, Bool)] -> TruthTable
tableFromList ls = foldr (uncurry setRow) emptyTable ls

allTrueTable, allFalseTable :: Set.Set Variable
              -> TruthTable
allFalseTable = allSetTable False
allTrueTable = allSetTable True

allSetTable :: Bool -> Set.Set Variable -> TruthTable
allSetTable b varSet =
    let allAssignments = allBoolCombinations varSet
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
