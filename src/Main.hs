module Main where

import Arithmetics
import CoinCBCInterface
import Formula
import MinimizeFormula
import NormalForm
import Qm
import Variable

import System.Environment(getArgs)
import Text.Printf(printf)
import Data.List(sort)
import Control.Monad(forM_, when)
import qualified Data.Set as Set
import System.Info(os)
import System.Process(readProcess)

import Test.Hspec
import Test.QuickCheck
import VariableSpec
import QmcCpp


main :: IO ()
main = do
    args <- getArgs
    let bitWidth = read (head args) :: Int
    runCBC bitWidth


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

minimizeFormulaExt :: Formula -> IO Formula
minimizeFormulaExt formula =
    let canonical = ensureCanonical formula
        cnfMode = (getType canonical == CNFType)
        varSet = variableSet formula
        numVars = Set.size varSet
        ones = canonicalToBitVectors varSet canonical
    in do
        putStrLn $ show (length ones) ++ " ones."
        let primes = qmcCppComputePrimes ones
        putStrLn $ "Found " ++ show (length primes) ++ " primes."
        putStrLn "Validating that a formula created from the primes has the same value as the original one (for random assignments)..."

        let primesFormula = qmTermsToFormula varSet cnfMode primes
        quickCheck $ property $ \assignment ->
            let assignment' = expandOrReduce False varSet assignment
            in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` primesFormula

        let lpFileContents = toLPFile numVars primes ones
        let lpFileName = "bitblast-cbctemp.lp"
        writeFile lpFileName lpFileContents
        let cbcSolutionFileName = "bitblast-cbcsolution.txt"
        let cbcCommands = "import " ++ lpFileName ++ "\nbranchAndCut" ++ "\nsolution " ++ cbcSolutionFileName ++ "\n"
        putStrLn "Run CBC in the current working directory and execute these commands:"
        putStrLn ""
        putStrLn cbcCommands
        copyToClipboard cbcCommands
        putStrLn "After the solution file has been written, press Enter to continue ..."
        getLine
        cbcSolution <- readFile cbcSolutionFileName
        let essentialPrimeIndices = parseSolution cbcSolution
        let essentialPrimes = map snd $ filter (\(i,_) -> i `elem` essentialPrimeIndices) $ zip [0..] primes
        return $ qmTermsToFormula varSet cnfMode essentialPrimes


-- | Only on OSX for now
copyToClipboard :: String -> IO ()
copyToClipboard str = do
    when (os == "darwin") $ do
        readProcess "pbcopy" [] str
        putStrLn "The commands have been copied to the clipboard."

    
