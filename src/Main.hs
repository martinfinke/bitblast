module Main where

import Assignment
import Arithmetics
import ArithmeticsModular
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

import System.Environment(getArgs, withArgs)
import qualified Data.Set as Set
import Data.List(sortBy)
import Data.Ord(comparing)
import Text.Printf(printf)
import Control.Monad(void)

import Test.Hspec
import Test.QuickCheck
import VariableSpec

vars@[v0,v1,v2,v3,v4,v5,v6,v7,v8,v9] = makeVars 10
varSet = Set.fromList vars
[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = map Atom vars

addition3ReplaceFirstCarry = fmap fst $ minimizeByReplacing minimizeFormula [And [x0, x3]] $ nBitAddition Forbid 3
addition3ReplaceSecondCarry = fmap fst $ minimizeByReplacing minimizeFormula [And [x1, x4]] $ nBitAddition Forbid 3
addition4ReplaceFirstCarry = fmap fst $ minimizeByReplacing minimizeFormula [And [x0, x4]] $ nBitAddition Forbid 4
addition5ReplaceFirstCarry = fmap fst $ minimizeByReplacing minimizeFormula [And [x0, x5]] $ nBitAddition Forbid 5



testTerm1 = Implies x4 x3
testTerm2 = Equiv [x3, Equiv [Or [x1, x0], x4]]
testTerm3 = Equiv [x4, Xor [x0,x2]]

originalF = Implies (Xor [x0,x2]) (Equiv [Or [x1,x0], Xor [x0,x2]])
f = And $ concatMap (removeAnd . getFormula . toCanonicalCnf) [testTerm1, testTerm2, testTerm3]
    where removeAnd (And fs) = fs

main = do
    args <- getArgs
    let mode = args!!0
    let circuitType = args!!1
    let numBits = read (args!!2)
    let circuit
            | circuitType == "add_forbid" = nBitAddition Forbid numBits
            | circuitType == "add_dontcare" = nBitAddition DontCare numBits
            | circuitType == "mul_forbid" = nBitMultiplication Forbid numBits
            | circuitType == "mul_dontcare" = nBitMultiplication DontCare numBits
            | circuitType == "gt" = greaterThan numBits
            | circuitType == "ge" = greaterThanEq numBits
    case mode of
        "min" -> minimize circuitType circuit numBits
        "min_struct" -> minimizeStruct circuitType circuit numBits args
        "min_truth" -> minimizeTruth circuitType circuit numBits args
        "min_genetic" -> minimizeGen circuitType circuit numBits args
        "table" -> table circuitType circuit numBits args
        "test_comb" -> testComb
        _ -> worthExtraVars args
    where minimize circuitType circuit numBits = do
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            optimized <- minimizeFormula circuit
            putStrLn $ "Minimized formula:"
            putStrLn . Formula.prettyPrint $ optimized
            print optimized
            print $ getStats optimized
          minimizeStruct circuitType circuit numBits args = do
            let extraVars = read (args!!3)
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            optimized <- fmap fst $ minimizeStructural extraVars circuit
            putStrLn $ printf "Smallest formula for k=%d:" extraVars
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
          minimizeGen circuitType circuit numBits args = do
            let extraVars = read (args!!3)
            putStrLn $ printf "Optimizing %s (%d bit)..." circuitType numBits
            void $ minimizeGeneticEndless extraVars circuit
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

testComb = withArgs [] $ hspec $ do
    describe "combineAdd" $ do
        let finalize numBits = forbidOverflow numBits . noCarryIn numBits
        it "returns a circuit that is equisatGTE to a circuit that was created in one piece" $ do
            let bitGroups = [
                      (1,1)
                    , (1,2)
                    , (2,1)
                    , (2,2)
                    ]
            let check (lBits,hBits) =
                    let numBits = lBits + hBits
                        onePiece = nBitAddition Forbid numBits
                        lhs = summerModule lBits
                        rhs = summerModule hBits
                        comb = combineAdd (lBits,hBits) lhs rhs
                        result = finalize numBits comb `equisatGTE` onePiece
                    in result `shouldBe` True
            mapM_ check bitGroups

        it "works when combining two circuits that were combined from circuits" $ do
            let onePiece = nBitAddition Forbid 4
                oneBit = summerModule 1
                twoBit = combineAdd (1,1) oneBit oneBit
                comb = combineAdd (2,2) twoBit twoBit
            --finalize 4 comb `equisatGTE` onePiece `shouldBe` True
            pending

    describe "combineMul" $ do
        it "returns a circuit that is equisatGTE to a circuit that was created in one piece" $ do
            let bitGroups = [
                      (1,1)
                    --, (1,2)
                    --, (2,1)
                    --, (2,2)
                    --, (2,3)
                    --, (3,2)
                    ]
            let check (lBits,hBits) =
                    let numBits = lBits + hBits
                        onePiece = nBitMultiplication ToSum numBits
                        multiplier = multiplierModule lBits
                        comb = combineMul numBits multiplier
                        result = comb `equisatGTE` onePiece
                    in result `shouldBe` True
            mapM_ check bitGroups
