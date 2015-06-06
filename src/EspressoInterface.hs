module EspressoInterface where

import Qm(QmTerm(..), BitVector, fromString, printTerm)
import System.Process(readProcess)
import Debug.Trace(traceShow)

newtype PLA = PLA String
    deriving(Eq)

instance Show PLA where
    show (PLA string) = show string

toPLA :: Int -> [QmTerm] -> PLA
toPLA numVariables qmTerms =
    let header = unlines [".i " ++ show numVariables, ".o 1"]
        footer = ".e\n"
    in PLA $ header ++ unlines (map (printTerm numVariables) qmTerms) ++ footer

parseOutput :: String -> [QmTerm]
parseOutput output =
    let ls = lines output
        withoutHeader = dropWhile (\line -> head line == '.') ls
        terms = takeWhile (\line -> head line /= '.') withoutHeader
        withoutOne = map (init . init) terms
    in map fromString withoutOne

runEspresso :: PLA -> [String] -> IO String
runEspresso (PLA string) args = do
    -- First, perform a quick distance-1-merge
    -- This is useful because the input generally has a lot of terms
    distance1Merge <- readProcess "espresso" ["-Dd1merge"] string
    readProcess "espresso" args distance1Merge

espressoGetPrimes :: Int -> [BitVector] -> IO String
espressoGetPrimes numVariables ones = do
    let onesQm = map (\one -> QmTerm(one,0)) ones
    let pla = toPLA numVariables onesQm
    output <- runEspresso pla ["-eness", "-Dexact"]
    return output

espressoOptimizeDefault, espressoOptimizeExact :: Int -> [QmTerm] -> IO [QmTerm]
espressoOptimizeDefault = espressoOptimizeWith []

espressoOptimizeExact = espressoOptimizeWith ["-Dexact"]

espressoOptimizeWith :: [String] -> Int -> [QmTerm] -> IO [QmTerm]
espressoOptimizeWith args numVariables terms = do
    let pla = toPLA numVariables terms
    fmap parseOutput (runEspresso pla args)