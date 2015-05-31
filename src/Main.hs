module Main where

import Arithmetics
import EspressoInterface
import Formula
import MinimizeFormula
import NormalForm
import Qm
import TruthTable

import System.Environment(getArgs)
import Text.Printf(printf)
import Control.Monad(forM_)
import qualified Data.Set as Set

nBitAddition :: OverflowMode -> Int -> Formula
nBitAddition overflowMode numBits =
    -- Variable ordering is [x2,x1,x0] + [x5,x4,x3] = [x8,x7,x6]
    let first = map (Atom . var) $ reverse $ [0..numBits-1]
        second = map (Atom . var) $ reverse $ [numBits..2*numBits-1]
        sums = map (Atom . var) $ reverse $ [2*numBits..3*numBits-1]
        summerCircuit = summer overflowMode first second sums
    in summerCircuit

main :: IO ()
main = do
    args <- getArgs
    let bitWidth = read (head args) :: Int
    runEspressoVerbose bitWidth
    
runVerbose :: Int -> IO ()
runVerbose numBits = do
    let addition = nBitAddition Forbid numBits
    putStrLn "Circuit:"
    putStrLn $ show addition
    putStrLn "----------------------------------------------------------------"
    let oneTerms = canonicalToBitVectors (ensureCanonical addition)
    putStrLn "Ones:"
    putStrLn $ show oneTerms
    putStrLn "----------------------------------------------------------------"
    let (numvars,ones,dc) = prepareInput oneTerms [] []
    putStrLn "(numvars,ones,dc):"
    putStrLn $ show (numvars,ones,dc)
    putStrLn "----------------------------------------------------------------"
    let primes = Set.toList $ compute_primes (Set.union ones dc)
    putStrLn "Primes:"
    putStrLn $ show $ map printAsNumbers primes
    putStrLn "----------------------------------------------------------------"
    let covers = prepareCovers primes (Set.toAscList ones)
    putStrLn "Covers:"
    putStrLn $ show covers
    putStrLn "----------------------------------------------------------------"
    let minimized = minimize_complexity numvars primes covers
    putStrLn "Minimized:"
    putStrLn $ show minimized
    putStrLn "----------------------------------------------------------------"

runEspressoVerbose :: Int -> IO ()
runEspressoVerbose numBits = do
    let addition = nBitAddition Forbid numBits
    --putStrLn "Circuit:"
    --putStrLn $ show addition
    --putStrLn "----------------------------------------------------------------"
    let onesQm = map (\one -> QmTerm(one,0)) $ canonicalToBitVectors (ensureCanonical addition)
    putStrLn $ "Ones: " ++ (show $ length onesQm)
    optimizedTerms <- espressoOptimizeExact (3*numBits) $ onesQm
    putStrLn $ "Optimized: " ++ (show $ length optimizedTerms)
    let cnf = qmTermsToFormula True (3*numBits) optimizedTerms
    putStrLn "CNF:"
    putStrLn $ show cnf
    putStrLn "----------------------------------------------------------------"
    putStrLn $ show (getStats cnf)
