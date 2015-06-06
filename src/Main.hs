module Main where

import Arithmetics
import EspressoInterface
import CoinCBCInterface
import Formula
import MinimizeFormula
import NormalForm
import Qm
import Variable hiding(eval)

import System.Environment(getArgs)
import Text.Printf(printf)
import Data.List(sort)
import Control.Monad(forM_)
import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck
import VariableSpec
import QmcCpp



-- | Minimizes a formula using Espresso and Coin CBC
minimizeFormulaExt :: Formula -> IO Formula
minimizeFormulaExt formula =
    let canonical = ensureCanonical formula
        cnfMode = (getType canonical == CNFType)
        varSet = variableSet formula
        numVars = Set.size varSet
        ones = canonicalToBitVectors varSet canonical
    in do
        --putStrLn $ "Ones: " ++ show ones
        let primes = qmcCppComputePrimes ones
        -- compute_primes is slower than espresso, so use espresso:
        --primes' <- fmap parseOutput $ espressoGetPrimes numVars ones
        --let primes' = Set.toList $ compute_primes (Set.fromList ones)

        putStrLn $ "Found " ++ show (length primes) ++ " primes."
        --putStrLn $ if Set.fromList primes == Set.fromList primes'
        --    then "OK: Primes from C++ are as they should be."
        --    else "ERROR: Primes from C++ are wrong!\nPrimes from qmcCpp:\n" ++ show (sort primes) ++ "\n\nShould be:\n" ++ show (sort primes')
        putStrLn "Validating that a formula created from the primes has the same value as the original one (for random assignments)..."

        let primesFormula = qmTermsToFormula varSet cnfMode primes
        quickCheck $ property $ \assignment ->
                        let assignment' = expandOrReduce False varSet assignment
                        in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` primesFormula

        let lpFileContents = toLPFile numVars primes ones
        let lpFileName = "bitblast-cbctemp.lp"
        writeFile lpFileName lpFileContents
        let cbcSolutionFileName = "bitblast-cbcsolution.txt"
        putStrLn "Run CBC in the current working directory and execute these commands:"
        putStrLn $ ""
        putStrLn $ "import " ++ lpFileName
        putStrLn "branchAndCut"
        putStrLn $ "solution " ++ cbcSolutionFileName
        putStrLn $ ""
        putStrLn "After the solution file has been written, press Enter to continue ..."
        getLine
        cbcSolution <- readFile cbcSolutionFileName
        let essentialPrimeIndices = parseSolution cbcSolution
        let essentialPrimes = map snd $ filter (\(i,_) -> i `elem` essentialPrimeIndices) $ zip [0..] primes

        return $ qmTermsToFormula varSet cnfMode essentialPrimes

runCBC :: Int -> IO ()
runCBC numBits = do
    --let (formula,varSet) = nBitAddition Forbid numBits
    let (formula,varSet) = nBitMultiplication numBits
    cnf <- minimizeFormulaExt formula


    putStrLn "CNF:"
    putStrLn $ show cnf
    putStrLn "----------------------------------------------------------------"
    putStrLn $ show (getStats cnf)
    putStrLn "----------------------------------------------------------------"
    quickCheck $ property $ \assignment ->
                    let assignment' = expandOrReduce False varSet assignment
                    in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` cnf



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
    runCBC bitWidth
    
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
