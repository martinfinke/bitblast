module Variable where

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.IntMap as IntMap

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
    deriving(Eq, Show)

emptyAssignment :: Assignment
emptyAssignment = Assignment IntMap.empty

getVarValue :: Variable -> Assignment -> Maybe Bool
getVarValue (Variable i) (Assignment intMap) = IntMap.lookup i intMap

setVarValue :: Variable -> Bool -> Assignment -> Assignment
setVarValue (Variable i) b (Assignment intMap) = Assignment $ IntMap.insert i b intMap





addVariable :: VarMem -> VarMem
addVariable mem@(VarMem {currentVarIndex=idx}) = mem{currentVarIndex=succ idx}