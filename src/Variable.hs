module Variable where

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

newtype VarMem = VarMem {currentVarIndex :: Int}
type VarState = State.State VarMem
type VarStateTransformer = State.StateT VarMem
newtype Variable = Variable Int
    deriving(Eq)

initial :: VarMem
initial = VarMem {currentVarIndex=0}

eval :: VarMem -> VarState a -> a
eval = flip State.evalState

var :: Monad m => String -> VarStateTransformer m Variable
var varName = do
    varIndex <- fmap currentVarIndex State.get
    State.modify addVariable
    return $ Variable varIndex

newtype Assignment = Assignment (IntMap.IntMap Bool)
    deriving(Eq, Show, Ord)

emptyAssignment :: Assignment
emptyAssignment = Assignment IntMap.empty

getVarValue :: Variable -> Assignment -> Maybe Bool
getVarValue (Variable i) (Assignment intMap) = IntMap.lookup i intMap

setVarValue :: Variable -> Bool -> Assignment -> Assignment
setVarValue (Variable i) b (Assignment intMap) = Assignment $ IntMap.insert i b intMap

newtype TruthTable = TruthTable (Map.Map Assignment Bool)
    deriving(Eq)

emptyTable :: TruthTable
emptyTable = TruthTable (Map.empty)

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

addVariable :: VarMem -> VarMem
addVariable mem@(VarMem {currentVarIndex=idx}) = mem{currentVarIndex=succ idx}