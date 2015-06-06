module CoinCBCInterface where

import QmcTypes(primeCoversOne,BitVector,QmTerm,invertedNumvarsMask,primeComplexity)
import Variable
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
        cols = Map.fromList [(one, [i | (i,prime) <- indexedPrimes, primeCoversOne prime one]) | one <- ones]
        uncoveredMinterms = Map.filter null cols
    in if Map.null uncoveredMinterms
        then cols
        else error $ "Uncovered minterms: " ++ show (Map.keys uncoveredMinterms)

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
        binary = ' ' : intercalate " " variableNames
        contents = unlines [
            "Minimize",
            objective,
            "Subject To",
            subjectTo,
            "binary",
            binary,
            "End"
            ]
    in contents

