module Main where

import Arithmetics
import EspressoInterface
import Formula
import MinimizeFormula
import NormalForm
import Qm
import Variable hiding(eval)

import System.Environment(getArgs)
import Text.Printf(printf)
import Control.Monad(forM_)
import qualified Data.Set as Set

nBitAddition :: OverflowMode -> Int -> (Formula, Set.Set Variable)
nBitAddition overflowMode numBits =
    -- Variable ordering is [x2,x1,x0] + [x5,x4,x3] = [x8,x7,x6]
    let vars = makeVars (3*numBits)
        atoms = map Atom vars
        first = reverse $ take numBits atoms
        second = reverse $ take numBits $ drop numBits atoms
        sums = reverse $ take numBits $ drop (2*numBits) atoms
        summerCircuit = summer overflowMode first second sums
    in (summerCircuit,Set.fromList vars)

nBitMultiplication :: Int -> (Formula, Set.Set Variable)
nBitMultiplication numBits = 
    let vars = makeVars (3*numBits)
        atoms = map Atom vars
        first = reverse $ take numBits atoms
        second = reverse $ take numBits $ drop numBits atoms
        sums = reverse $ take numBits $ drop (2*numBits) atoms
        multiplierCircuit = multiplier first second sums
    in (multiplierCircuit,Set.fromList vars)

main :: IO ()
main = do
    args <- getArgs
    let bitWidth = read (head args) :: Int
    runEspressoVerbose bitWidth
    
runVerbose :: Int -> IO ()
runVerbose numBits = do
    let (addition,varSet) = nBitAddition Forbid numBits
    putStrLn "Circuit:"
    putStrLn $ show addition
    putStrLn "----------------------------------------------------------------"
    let oneTerms = canonicalToBitVectors varSet (ensureCanonical addition)
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
    let (addition,additionVarSet) = nBitAddition DontCare numBits
    let (multiplication,multiplicationVarSet) = nBitMultiplication numBits

    putStrLn $ printf "Optimizing %d-bit Addition …" numBits
    runWith addition additionVarSet

    --putStrLn $ printf "Optimizing %d-bit Multiplication …" numBits
    --runWith multiplication multiplicationVarSet

    where runWith circuit varSet = do
            let onesQm = map (\one -> QmTerm(one,0)) $ canonicalToBitVectors varSet (ensureCanonical circuit)
            putStrLn $ "Ones: " ++ (show $ length onesQm)
            optimizedTerms <- espressoOptimizeExact (3*numBits) $ onesQm
            putStrLn $ "Optimized: " ++ (show $ length optimizedTerms)
            let cnf = qmTermsToFormula varSet True optimizedTerms
            putStrLn "CNF:"
            putStrLn $ show cnf
            putStrLn "----------------------------------------------------------------"
            putStrLn $ show (getStats cnf)

