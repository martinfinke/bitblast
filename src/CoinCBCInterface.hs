module CoinCBCInterface where

import Qm
import Variable
import MinimizeFormula
import NormalForm
import Formula

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List(intercalate)
import System.Process
import System.IO.Temp
import System.IO(hPutStr)
import System.Directory(getTemporaryDirectory)
import System.FilePath
import Text.Regex.Posix
import Debug.Trace(traceShow)
import Data.Char(isDigit)

columns :: [QmTerm] -> [BitVector] -> Map.Map BitVector [Int]
columns primes ones =
    let indexedPrimes = zip [0..] primes
        columns = Map.fromList [(one, [i | (i,prime) <- indexedPrimes, primeCoversOne prime one]) | one <- ones]
        uncoveredMinterms = Map.filter null columns
    in if Map.null uncoveredMinterms
        then columns
        else error $ "Uncovered minterms: " ++ show (Map.keys uncoveredMinterms)


testLPFile = "Maximize\n obj: x1 + 2 x2 + 3 x3 + x4\nSubject To\n c1: - x1 + x2 + x3 + 10 x4 <= 20\n c2: x1 - 3 x2 + x3 <= 30\n c3: x2 - 3.5 x4 = 0\nBounds\n 0 <= x1 <= 40\n 2 <= x4 <= 3\nGeneral\n x4\nEnd"

testPrimes = map fromString ["0-01", "-010", "01--"]
testOnes = map (getTerm . fromString) ["0101", "0010"]


testSolution = "Optimal - objective value 32.00000000\n      0 x0                     1                      4\n      1 x1                     1                      3\n      2 x2                     1                      3\n      3 x3                     1                      3\n      4 x4                     1                      4\n      5 x5                     1                      3\n      6 x6                     1                      3\n      7 x7                     1                      3\n      8 x8                     1                      2\n      9 x9                     1                      2\n     10 x10                    1                      2\n"


parseSolution :: String -> [Int]
parseSolution solutionOutput =
    let ls = lines solutionOutput
        withoutHead = tail ls
        ints = map parseSolutionLine withoutHead
    in ints

parseSolutionLine :: String -> Int
parseSolutionLine line =
    let (_,_,numberAndRest) = line =~ "[^x]*x" :: (String,String,String)
        number = takeWhile isDigit numberAndRest
    in read number

toLPFile :: Int -> [QmTerm] -> [BitVector] -> String
toLPFile numVars primes ones =
    let cols = columns primes ones
        mask = invertedNumvarsMask numVars
        prices = map (primeComplexity mask) primes
        variableNames = take (length primes) $ map ((:)'x' . show) [0..]
        objective = " obj: " ++ intercalate " + " (map (\(p,v) -> show p ++ " " ++ v) (zip prices variableNames))
        subjectTo = unlines . reverse . fst $ Map.foldrWithKey (\one primes (list, i) ->
                                      let line = " c" ++ show i ++ ": x" ++ intercalate " + x" (map show primes) ++ " >= 1" in (line : list, i+1)
                                      ) ([],1) cols
        bounds = unlines $ map (\v -> " 0 <= " ++ v ++ " <= 1") variableNames
        binary = ' ' : intercalate " " variableNames
        contents = unlines [
            "Minimize",
            objective,
            "Subject To",
            subjectTo,
            --"Bounds",
            --bounds,
            "binary",
            binary,
            "End"
            ]
    in contents

