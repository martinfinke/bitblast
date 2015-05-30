module Main where

import Arithmetics
import Formula
import MinimizeFormula
import NormalForm
import TruthTable

import Text.Printf(printf)
import Control.Monad(forM_)

nBitAddition :: OverflowMode -> Int -> Formula
nBitAddition overflowMode numBits =
    let numVars = 3*numBits
        vars = map (Atom . var) [0..numVars-1]
        (xs,vars') = splitAt numBits vars
        (ys,sums) = splitAt numBits vars'
        summerCircuit = summer overflowMode xs ys sums
    in summerCircuit

main :: IO ()
main = do
    let bits = [1..5]
    forM_ bits $ \bitWidth -> do
        putStrLn $ printf "------------ %d-Bit Addition: --------------" bitWidth
        putStrLn          "--------------------------------------------"
        let circuit = nBitAddition DontCare bitWidth
        putStrLn $ "Circuit:"
        putStrLn $ show circuit
        let minimized = minimizeFormula circuit
        putStrLn $ "CNF " ++ show (getStats minimized)
        putStrLn $ show minimized
        putStrLn "\n\n\n"