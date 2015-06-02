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
                getVar,
                setVar,
                TruthTable,
                emptyTable,
                getRow,
                setRow,
                tableFromList
                ) where

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(forM)

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

getVar :: Variable -> Assignment -> Maybe Bool
getVar (Variable i) (Assignment intMap) = IntMap.lookup i intMap

setVar :: Variable -> Bool -> Assignment -> Assignment
setVar (Variable i) b (Assignment intMap) = Assignment $ IntMap.insert i b intMap

newtype TruthTable = TruthTable (Map.Map Assignment Bool)
    deriving(Eq, Show)

emptyTable :: TruthTable
emptyTable = TruthTable Map.empty

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

tableFromList :: [(Assignment, Bool)] -> TruthTable
tableFromList ls = foldr (uncurry setRow) emptyTable ls
