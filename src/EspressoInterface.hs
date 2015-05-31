module EspressoInterface where

import Qm(QmTerm(..), BitVector, fromString)
import System.Process(readProcess)
import Debug.Trace(traceShow)

newtype PLA = PLA String
    deriving(Eq)

instance Show PLA where
    show (PLA string) = show string

toPLA :: Int -> [QmTerm] -> PLA
toPLA numVariables qmTerms =
    let header = unlines [".i " ++ show numVariables, ".o 1"]
        printTerm qmTerm = (replicate (numVariables - (length $ show qmTerm)) '0') ++ show qmTerm ++ " 1"
        footer = ".e\n"
    in PLA $ header ++ unlines (map printTerm qmTerms) ++ footer

parseOutput :: String -> [QmTerm]
parseOutput output =
    let ls = lines output
        withoutHeader = dropWhile (\line -> head line == '.') ls
        terms = takeWhile (\line -> head line /= '.') withoutHeader
        withoutOne = map (init . init) terms
    in map fromString withoutOne

runEspresso :: PLA -> [String] -> IO String
runEspresso (PLA string) args = do
    readProcess "espresso" args string

espressoOptimizeDefault, espressoOptimizeExact :: Int -> [QmTerm] -> IO [QmTerm]
espressoOptimizeDefault = espressoOptimizeWith []

espressoOptimizeExact = espressoOptimizeWith ["-Dexact"]

espressoOptimizeWith :: [String] -> Int -> [QmTerm] -> IO [QmTerm]
espressoOptimizeWith args numVariables terms = do
    let pla = toPLA numVariables terms
    fmap parseOutput (runEspresso pla args)