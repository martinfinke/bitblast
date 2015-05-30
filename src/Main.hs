module Main where

import Arithmetics
import Formula
import MinimizeFormula
import NormalForm
import Qm
import TruthTable

import Text.Printf(printf)
import Control.Monad(forM_)

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
    let bits = [1..3]
    forM_ bits $ \bitWidth -> do
        putStrLn $ printf "------------ %d-Bit Addition: --------------" bitWidth
        putStrLn          "--------------------------------------------"
        let circuit = nBitAddition Forbid bitWidth
        putStrLn $ "Circuit:"
        putStrLn $ show circuit
        let minimized = minimizeFormula circuit
        putStrLn $ "CNF " ++ show (getStats minimized)
        putStrLn $ show minimized
        putStrLn "\n"