module Variable where

import qualified Control.Monad.Trans.State.Lazy as State

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





addVariable :: VarMem -> VarMem
addVariable mem@(VarMem {currentVarIndex=idx}) = VarMem {currentVarIndex=succ idx}