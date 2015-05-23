{-|
'Variable's, 'Assignment's and 'TruthTable's, along with functions to create, read and modify them.
-}
module TruthTable (
    Variable,
    maxNumVariables,
    var,
    Assignment,
    allFalse,
    allTrue,
    setVariable,
    setVariables,
    getVariable,
    TruthTable,
    emptyTable,
    numVariablesInTable,
    getOutput,
    setOutput,
    setOutputs,
    isValidAssignment
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Bits as B
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Text.Printf (printf)
import UnboxMaybe

-- | A boolean variable. Indexed using 'Int's ranging from 0 to ('maxNumVariables'-1).
newtype Variable = Variable Int
    deriving(Eq,Ord)

-- | The maximum number of variables.
maxNumVariables :: Int
maxNumVariables = B.popCount (maxBound::Int)

instance Bounded Variable where
    minBound = Variable 0
    maxBound = Variable (maxNumVariables-1)

instance Enum Variable where
    fromEnum (Variable i) = i
    toEnum = var

instance Show Variable where
    show (Variable i) = show i

-- | Creates a new 'Variable'.
var :: Int -- ^ The index for the new 'Variable'. Has to be >= 0 and < 'maxNumVariables', otherwise it will throw an error.
    -> Variable
var i   | Variable i < minBound = error $ printf "Negative variable index (%d) is not allowed" i
        | Variable i > maxBound = error $ printf "Variable index too high: %d > %d" i (fromEnum (maxBound::Variable))
        | otherwise = Variable i

-- | Assigns a boolean value to each 'Variable'.
newtype Assignment = Assignment Int
    deriving(Eq)

instance Bounded Assignment where
    minBound = allFalse
    maxBound = allTrue

instance Enum Assignment where
    fromEnum (Assignment bits) = bits
    toEnum = Assignment

instance Show Assignment where
    show (Assignment bits) = printf formatString $ showIntAtBase 2 intToDigit bits ""
        where formatString = "%0" ++ show maxNumVariables ++ "s"

allFalse, allTrue :: Assignment
-- | An 'Assignment' with every 'Variable' set to false.
allFalse = Assignment B.zeroBits
-- | An 'Assignment' with every 'Variable' set to true.
allTrue = setVariables (zip [minBound..maxBound::Variable] (repeat True)) allFalse

-- | Sets a single 'Variable' in an 'Assignment' to 'True' or 'False'.
setVariable :: Variable -- ^ The 'Variable' to set
            -> Bool -- ^ The value to set to
            -> Assignment -- ^ The input 'Assignment'
            -> Assignment -- ^ The 'Assignment' with the variable set to the new value
setVariable (Variable index) isTrue (Assignment bits) = Assignment $ bits `operation` index
    where operation = if isTrue then B.setBit else B.clearBit

-- | Sets multiple 'Variables' in an 'Assignment' at once.
setVariables :: [(Variable, Bool)] -- ^ List of variable/value pairs to set
             -> Assignment -- ^ The input 'Assignment'
             -> Assignment -- ^ The 'Assignment' with each variable set to its new value
setVariables varsWithValues assignment = foldr setValue assignment varsWithValues
    where setValue (variable, value) a = setVariable variable value a

-- | Retrieve the value of a 'Variable' in an 'Assignment'
getVariable :: Variable
            -> Assignment
            -> Bool
getVariable (Variable index) (Assignment bits) = B.testBit bits index

-- | A 'TruthTable' is a mapping from 'Assignment's to a 'Bool', or 'Nothing' (don't care).
newtype TruthTable = TruthTable (V.Vector (Maybe Bool))
    deriving (Eq)

instance Show TruthTable where
    show table@(TruthTable outputColumn) = unlines $ map trim $ V.ifoldr appendRow [] outputColumn
        where appendRow rowIndex outputValue rowStrings = renderRow rowIndex outputValue:rowStrings
              numVariables = numVariablesInTable table
              trim = drop (maxNumVariables-numVariables)

renderRow :: Int -> Maybe Bool -> String
renderRow rowIndex outputValue = show (assignments!!rowIndex) ++ " " ++ renderOutputValue outputValue
    where assignments = [minBound..maxBound::Assignment]
          renderOutputValue v = case v of
            Just True -> "1"
            Just False -> "0"
            Nothing -> "-"

-- | Creates a 'TruthTable' for a given number of 'Variable's. All outputs are initially set to 'Nothing' (don't care).
emptyTable :: Int -- ^ The number of 'Variable's used as input for the table. Has to be <= 'maxNumVariables'. The table size will be 2^thisValue.
           -> TruthTable
emptyTable numVariables
    | numVariables <= maxNumVariables = TruthTable $ V.replicate (2^numVariables) Nothing
    | otherwise = error $ "Can't create TruthTable with too many variables (" ++ show numVariables ++ ")"

-- | The number of variables a given 'TruthTable' can hold.
numVariablesInTable :: TruthTable -> Int
numVariablesInTable (TruthTable outputColumn)
    | len == 0 = 0
    | otherwise = B.popCount (len-1)
    where len = V.length outputColumn

-- | Gets the output of a given 'Assignment' (i.e. row) in a 'TruthTable'.
getOutput :: Assignment -> TruthTable -> Maybe Bool
getOutput (Assignment index) (TruthTable outputColumn) = case outputColumn V.!? index of
    Nothing -> error $ printIndexError index (V.length outputColumn)
    Just internal -> internal

-- | Sets the output of a given 'Assignment' (i.e. row) in a 'TruthTable'.
setOutput :: Assignment -> Maybe Bool -> TruthTable -> TruthTable
setOutput (Assignment index) newValue (TruthTable outputColumn)
    | index >= (V.length outputColumn) = error $ printIndexError index (V.length outputColumn)
    | otherwise = TruthTable (outputColumn V.// [(index, newValue)])

-- | Sets multiple outputs at once.
setOutputs :: [(Assignment, Maybe Bool)] -> TruthTable -> TruthTable
setOutputs outputValues truthTable = foldr (\(assignment, outputValue) table -> setOutput assignment outputValue table) truthTable outputValues

-- | Checks whether an 'Assignment' is valid (i.e. not out of bounds) for a given 'TruthTable'. This is true iff all its true variables' indices are <= the value that was passed to 'emptyTable'.
isValidAssignment :: Assignment -> TruthTable -> Bool
isValidAssignment (Assignment index) (TruthTable outputColumn) = index < V.length outputColumn

printIndexError :: Int -> Int -> String
printIndexError index len = printf "Index out of range: %d >= %d" index len
