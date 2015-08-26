module Main where

import Assignment
import Arithmetics
import Formula
import CalculatedFormulas
import EspressoInterface
import LimpCBCInterface
import MinimizeFormula
import NormalForm
import ParseCnf
import QmcCpp
import QmcTypes
import SatchmoInterface
import SatchmoOutput
import TruthBasedCore
import TruthBased
import TruthBasedApprox
import TruthBasedGenetic
import TruthTable
import Tseitin
import TseitinSelect
import Variable
import Utils
import WorthExtraVariables

import System.Environment(getArgs)
import qualified Data.Set as Set
import Data.List(sortBy)
import Data.Ord(comparing)
import Text.Printf(printf)

import Test.Hspec
import Test.QuickCheck
import VariableSpec

vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = makeVars 10
varSet = Set.fromList vars
[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars

-- ((-0 || 2 || 3) && (-0 || 1 || 3) && (-0 || 1 || 2) && (0 || -1 || 2) && (0 || 1 || -2) && (-1 || -2 || -3))
-- (\neg x0 \lor x2 \lor x3) \land (\neg x0 \lor x1 \lor x3) \land (\neg x0 \lor x1 \lor x2) \land (x0 \lor \neg x1 \lor x2) \land (x0 \lor x1 \lor \neg x2) \land (\neg x0 \lor \neg x2 \lor \neg x3)
testF = And [Or [Not x0, x2, x3], Or [Not x0, x1, x3], Or [Not x0, x1, x2], Or [x0, Not x1, x2], Or [x0, x1, Not x2], Or [Not x1, Not x2, Not x3]]

main = do
    args <- getArgs
    let mode = args!!0
    let circuitType = args!!1
    let numBits = read (args!!2)
    let circuit
            | circuitType == "add_forbid" = nBitAddition Forbid numBits
            | circuitType == "add_dontcare" = nBitAddition DontCare numBits
            | circuitType == "mul_forbid" = getFormula $ multiplication Forbid numBits
            | circuitType == "mul_dontcare" = getFormula $ multiplication DontCare numBits
            | circuitType == "gt" = getFormula $ greaterThan numBits
            | circuitType == "ge" = getFormula $ greaterThanEq numBits
    case mode of
        "min" -> minimize circuitType circuit numBits
        "min_truth" -> minimizeTruth circuitType circuit numBits args
        "table" -> table circuitType circuit numBits args
        _ -> worthExtraVars args
    where minimize circuitType circuit numBits = do
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            optimized <- minimizeFormula circuit
            putStrLn $ "Minimized formula:"
            putStrLn . Formula.prettyPrint $ optimized
            print optimized
            print $ getStats optimized
          minimizeTruth circuitType circuit numBits args = do
            let extraVars = read (args!!3)
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            optimized <- minimizeTruthBased extraVars circuit
            putStrLn $ printf "Smallest formula for k=%d:" extraVars
            putStrLn . Formula.prettyPrint $ optimized
            print optimized
            print $ getStats optimized
          table circuitType circuit numBits args = do
            let extraVars = read (args!!3)
            let clauseMode = read (args!!4) :: Bool
            putStrLn $ printf "Creating table for %s (%d bit)..." circuitType numBits
            let table = (if clauseMode == True then toTableQmc else toTable) extraVars circuit
            putStrLn "Table Info:"
            putStrLn (tableInfo table)
          worthExtraVars args = do
            let numVars = read $ args!!1
            smallestWorthExtra numVars

smallestWorthExtra numVars = do
    result <- findWorthExtra $ Set.fromList $ take numVars vars
    case result of
        Nothing -> putStrLn $ "There's no formula with " ++ show numVars ++ " variables for which it's worth introducing an extra variable."
        Just f -> do
            putStrLn $ "It's worth introducing an extra variable for this formula:"
            print f

xor = odd . length . filter id

---- ((0 || 1 || 2) && (0 || -1 || 2) && (0 || -1 || -2) && (-0 || 1 || 2) && (-0 || 1 || -2))
--smallestWorthExtra = And [
--    Or [x0,x1,x2],
--    Or [x0,Not x1,x2],
--    Or [x0,Not x1,Not x2],
--    Or [Not x0,x1,x2],
--    Or [Not x0,x1,Not x2]
--    ]
