module EspressoInterface where

import System.Process(readProcess)
import Debug.Trace(traceShow)
import TruthBasedCore(Assignment, Clause(..), lit, assignments, CNF(..))

import Data.Maybe

newtype PLA = PLA String
    deriving(Eq)

instance Show PLA where
    show (PLA string) = show string

toPLA :: Int -> [Assignment] -> PLA
toPLA numVariables terms =
    let header = unlines [
            ".i " ++ show numVariables, ".o 1",
            ".p " ++ show (length terms)
            ]
        printBool b = if b then '1' else '0'
        printAssignment output a = map printBool a ++ " " ++ [output]
        termLines = map (printAssignment '1') terms
        footer = ".e\n"
    in PLA $ header ++ unlines termLines ++ footer

parseOutput :: String -> CNF
parseOutput output =
    let ls = lines output
        withoutHeader = dropWhile (\line -> head line == '.') ls
        terms = takeWhile (\line -> head line /= '.') withoutHeader
        withoutOne = map (init . init) terms
        readChar (i,ch) = case ch of
            '0' -> Just $ lit i True
            '1' -> Just $ lit i False
            '-' -> Nothing
            _ -> error "EspressoInterface.parseOutput: parse error!"
        fromString term = Clause . catMaybes $ map readChar $ zip [1..] term
    in CNF $ map fromString withoutOne

runEspresso :: PLA -> [String] -> IO String
runEspresso (PLA string) args = readProcess "espresso" args string

espressoOptimize, espressoOptimizeExact :: Int -> [Assignment] -> IO CNF
espressoOptimize = espressoOptimizeWith []

espressoOptimizeExact = espressoOptimizeWith ["-Dexact"]

espressoOptimizeWith :: [String] -> Int -> [Assignment] -> IO CNF
espressoOptimizeWith args numVariables zeros
    | null zeros = return $ CNF []
    | otherwise =
        let pla = toPLA numVariables zeros
        in fmap parseOutput (runEspresso pla args)
