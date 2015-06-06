module Main where

import Arithmetics
import CoinCBCInterface
import Formula
import MinimizeFormula
import NormalForm
import QmcCpp
import QmcTypes
import Tseitin
import TseitinSelect
import Variable

import System.Environment(getArgs)

import Test.Hspec
import Test.QuickCheck
import VariableSpec


main :: IO ()
main = do
    args <- getArgs
    let bitWidth = read (head args) :: Int
    runCBC bitWidth


runCBC :: Int -> IO ()
runCBC numBits = do
    --let (formula,varSet) = nBitAddition Forbid numBits
    let (formula,varSet) = nBitMultiplication numBits
    cnf <- minimizeFormula formula
    putStrLn "CNF:"
    putStrLn $ show cnf
    putStrLn "----------------------------------------------------------------"
    putStrLn $ show (getStats cnf)
    putStrLn "----------------------------------------------------------------"
    quickCheck $ property $ \assignment ->
        let assignment' = expandOrReduce False varSet assignment
        in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` cnf

