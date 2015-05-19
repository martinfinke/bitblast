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
allTrue = setVariables allFalse $ zip [minBound..maxBound::Variable] (repeat True)

setVariable :: Assignment -> Variable -> Bool -> Assignment
setVariable (Assignment bits) (Variable index) isTrue = Assignment $ bits `operation` index
    where operation = if isTrue then B.setBit else B.clearBit

setVariables :: Assignment -> [(Variable, Bool)] -> Assignment
setVariables assignment varsWithValues = foldr setValue assignment varsWithValues
    where setValue (variable, value) a = setVariable a variable value

getVariable :: Assignment -> Variable -> Bool
getVariable (Assignment bits) (Variable index) = B.testBit bits index

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

getOutput :: TruthTable -> Assignment -> OutputValue
getOutput (TruthTable outputColumn) (Assignment index) = case outputColumn V.!? index of
    Nothing -> error $ printIndexError index (V.length outputColumn)
    Just internal -> fromInternal internal

setOutput :: TruthTable -> Assignment -> OutputValue -> TruthTable
setOutput (TruthTable outputColumn) (Assignment index) newValue
    | index >= (V.length outputColumn) = error $ printIndexError index (V.length outputColumn)
    | otherwise = TruthTable (outputColumn V.// [(index, toInternal newValue)])

setOutputs :: TruthTable -> [(Assignment, OutputValue)] -> TruthTable
setOutputs = foldr $ \(assignment, outputValue) table -> setOutput table assignment outputValue

toInternal :: OutputValue -> InternalOutputValue
toInternal T = (True, True)
toInternal F = (True, False)
toInternal DC = (False, False)

fromInternal :: InternalOutputValue -> OutputValue
fromInternal (True, True) = T
fromInternal (True, False) = F
fromInternal (False, _) = DC

isValidAssignment :: TruthTable -> Assignment -> Bool
isValidAssignment (TruthTable outputColumn) (Assignment index) = index < V.length outputColumn

printIndexError :: Int -> Int -> String
printIndexError index len = printf "Index out of range: %d >= %d" index len

