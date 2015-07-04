module Main where

import Arithmetics
import Formula
import LimpCBCInterface
import MinimizeFormula
import NormalForm
import QmcCpp
import QmcTypes
import SatchmoInterface
import TruthBasedNaive
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
-- -(0 XOR 1) && ((0 XOR 1) <=> 2)
testF = And [Not $ Xor [x0,x1], Equiv [Xor [x0,x1], x2]]

main = do
    args <- getArgs
    let circuitType = args!!0
    let circuit
            | circuitType == "add_forbid" = nBitAddition Forbid
            | circuitType == "add_dontcare" = nBitAddition DontCare
            | circuitType == "mul" = nBitMultiplication
    let numBits = read (args!!1)
    let extraVarRange = read (args!!2) :: (Int,Int)
    putStrLn $ printf "Optimizing %s (%d bit)..." (circuitType) numBits
    let f = fst $ circuit numBits
    optimized <- minimizeTruthBasedWithExtraVarRange extraVarRange f
    putStrLn $ printf "Smallest formula for k `elem` [%d..%d]:" (fst extraVarRange) (snd extraVarRange)
    putStrLn $ show optimized

main' = do
    args <- getArgs
    let numVars = read $ args!!0
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