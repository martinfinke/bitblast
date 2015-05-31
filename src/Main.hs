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
    

runWithBitWidth :: Int -> IO ()
runWithBitWidth bitWidth = do
    putStrLn $ printf "------------ %d-Bit Addition: --------------" bitWidth
    putStrLn          "--------------------------------------------"
    let circuit = nBitAddition Forbid bitWidth
    putStrLn $ "Circuit:"
    putStrLn $ show circuit
    let minimized = minimizeFormula circuit
    putStrLn $ "CNF " ++ show (getStats minimized)
    putStrLn $ show minimized
    putStrLn "\n"

expectedPrimes = map QmTerm [(1,62), (1,300), (1,438), (2,125), (2,357), (2,364), (3,188), (4,251), (5,314), (6,377), (7,440), (8,55), (8,293), (8,438), (9,118), (9,356), (10,181), (11,244), (12,307), (13,370), (14,433), (15,496), (16,111), (16,357), (16,364), (17,174), (18,237), (19,300), (20,363), (21,426), (22,489), (24,167), (25,230), (26,293), (27,356), (28,419), (29,482), (32,223), (33,286), (34,349), (35,412), (36,475), (40,279), (41,342), (42,405), (43,468), (48,335), (49,398), (50,461), (56,391), (57,454), (64,438), (66,301), (73,438), (80,301), (128,357), (128,364), (131,300), (138,293), (139,356), (145,300), (146,357), (146,364), (152,293), (153,356), (192,301), (210,301), (256,195), (256,201), (256,202), (256,209), (256,216), (320,139), (320,153), (384,75), (384,83), (384,89), (384,90), (448,27)]

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
