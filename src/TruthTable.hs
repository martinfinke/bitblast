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
    OutputValue(..),
    TruthTable,
    emptyTable,
    numVariablesInTable,
    getOutput,
    setOutput,
    setOutputs,
    fromInternal,
    toInternal,
    isValidAssignment
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Bits as B
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Text.Printf (printf)

newtype Variable = Variable Int
    deriving(Eq,Ord)

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

var :: Int -> Variable
var i   | Variable i < minBound = error $ printf "Negative variable index (%d) is not allowed" i
        | Variable i > maxBound = error $ printf "Variable index too high: %d > %d" i (fromEnum (maxBound::Variable))
        | otherwise = Variable i

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
allFalse = Assignment B.zeroBits
allTrue = setVariables (zip [minBound..maxBound::Variable] (repeat True)) allFalse

setVariable :: Variable -> Bool -> Assignment -> Assignment
setVariable (Variable index) isTrue (Assignment bits) = Assignment $ bits `operation` index
    where operation = if isTrue then B.setBit else B.clearBit

setVariables :: [(Variable, Bool)] -> Assignment -> Assignment
setVariables varsWithValues assignment = foldr setValue assignment varsWithValues
    where setValue (variable, value) a = setVariable variable value a

getVariable :: Variable -> Assignment -> Bool
getVariable (Variable index) (Assignment bits) = B.testBit bits index

data OutputValue = T | F | DC
    deriving(Eq)

instance Show OutputValue where
    show T = "1"
    show F = "0"
    show DC = "-"

type InternalOutputValue = (Bool, Bool)

newtype TruthTable = TruthTable (V.Vector InternalOutputValue)

instance Show TruthTable where
    show table@(TruthTable outputColumn) = unlines $ map trim $ V.ifoldr appendRow [] outputColumn
        where appendRow rowIndex outputValue rowStrings = renderRow rowIndex outputValue:rowStrings
              numVariables = numVariablesInTable table
              trim = drop (maxNumVariables-numVariables)

renderRow :: Int -> InternalOutputValue -> String
renderRow rowIndex outputValue = show (assignments!!rowIndex) ++ " " ++ show (fromInternal outputValue)
    where assignments = [minBound..maxBound::Assignment]

emptyTable :: Int -> TruthTable
emptyTable numVariables
    | numVariables == 0 = TruthTable $ V.empty
    | numVariables <= maxNumVariables = TruthTable $ V.replicate (2^numVariables) (toInternal DC)
    | otherwise = error $ "Can't create TruthTable with too many variables (" ++ show numVariables ++ ")"

numVariablesInTable :: TruthTable -> Int
numVariablesInTable (TruthTable outputColumn)
    | len == 0 = 0
    | otherwise = B.popCount (len-1)
    where len = V.length outputColumn

getOutput :: Assignment -> TruthTable -> OutputValue
getOutput (Assignment index) (TruthTable outputColumn) = case outputColumn V.!? index of
    Nothing -> error $ printIndexError index (V.length outputColumn)
    Just internal -> fromInternal internal

setOutput :: Assignment -> OutputValue -> TruthTable -> TruthTable
setOutput (Assignment index) newValue (TruthTable outputColumn)
    | index >= (V.length outputColumn) = error $ printIndexError index (V.length outputColumn)
    | otherwise = TruthTable (outputColumn V.// [(index, toInternal newValue)])

setOutputs :: [(Assignment, OutputValue)] -> TruthTable -> TruthTable
setOutputs outputValues truthTable = foldr (\(assignment, outputValue) table -> setOutput assignment outputValue table) truthTable outputValues

toInternal :: OutputValue -> InternalOutputValue
toInternal T = (True, True)
toInternal F = (True, False)
toInternal DC = (False, False)

fromInternal :: InternalOutputValue -> OutputValue
fromInternal (True, True) = T
fromInternal (True, False) = F
fromInternal (False, _) = DC

isValidAssignment :: Assignment -> TruthTable -> Bool
isValidAssignment (Assignment index) (TruthTable outputColumn) = index < V.length outputColumn

printIndexError :: Int -> Int -> String
printIndexError index len = printf "Index out of range: %d >= %d" index len

