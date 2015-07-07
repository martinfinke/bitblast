module Main where

import Arithmetics
import Formula
import LimpCBCInterface
import MinimizeFormula
import NormalForm
import QmcCpp
import QmcTypes
import SatchmoInterface
import TruthBasedCore
import TruthBased
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
    if mode == "min" then minimize args else worthExtraVars args
    where minimize args = do
            let circuitType = args!!1
            let numBits = read (args!!2)
            let circuit
                    | circuitType == "add_forbid" = fst $ nBitAddition Forbid numBits
                    | circuitType == "add_dontcare" = fst $ nBitAddition DontCare numBits
                    | circuitType == "mul_forbid" = getFormula $ multiplication Forbid numBits
                    | circuitType == "mul_dontcare" = getFormula $ multiplication DontCare numBits
            let extraVars = read (args!!3)
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            optimized <- minimizeTruthBased extraVars circuit
            putStrLn $ printf "Smallest formula for k=%d:" extraVars
            putStrLn $ show optimized
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


---- ((0 || 1 || 2) && (0 || -1 || 2) && (0 || -1 || -2) && (-0 || 1 || 2) && (-0 || 1 || -2))
--smallestWorthExtra = And [
--    Or [x0,x1,x2],
--    Or [x0,Not x1,x2],
--    Or [x0,Not x1,Not x2],
--    Or [Not x0,x1,x2],
--    Or [Not x0,x1,Not x2]
--    ]