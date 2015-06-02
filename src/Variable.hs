module Variable(
                initial,
                eval,
                var,
                Assignment,
                emptyAssignment,
                assignmentFromList,
                getVar,
                setVar,
                TruthTable,
                emptyTable,
                getRow,
                setRow
                ) where

import qualified Control.Monad.Trans.State.Lazy as State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    deriving(Eq)

emptyTable :: TruthTable
emptyTable = TruthTable Map.empty

getRow :: Assignment -> TruthTable -> Maybe Bool
getRow assignment (TruthTable assignmentMap) = Map.lookup assignment assignmentMap

setRow :: Assignment -> Bool -> TruthTable -> TruthTable
setRow assignment b (TruthTable assignmentMap) = TruthTable $ Map.insert assignment b assignmentMap

tableFromList :: [(Assignment, Bool)] -> TruthTable
tableFromList ls = foldr (uncurry setRow) emptyTable ls

addVariable :: VarMem -> VarMem
addVariable mem@(VarMem {currentVarIndex=idx}) = mem{currentVarIndex=succ idx}