module Main where

import Arithmetics
import Formula
import MinimizeFormula
import NormalForm
import TruthTable

import Text.Printf(printf)
import Control.Monad(forM_)

nBitAddition :: Int -> Formula
nBitAddition numBits =
    let vars = map (Atom . var) [0..3*numBits + 1 - 1]
        (xs,vars') = splitAt numBits vars
        (ys,vars'') = splitAt numBits vars'
        cOut = head vars''
        sums = tail vars''
        summerCircuit = summer xs ys cOut sums
    in summerCircuit

main :: IO ()
main = do
    let bits = [1..5]
    forM_ bits $ \bitWidth -> do
        putStrLn $ printf "------------ %d-Bit Addition: --------------" bitWidth
        putStrLn          "--------------------------------------------"
        let circuit = nBitAddition bitWidth
        let minimized = minimizeFormula circuit
        putStrLn $ show minimized
        putStrLn $ "Stats: " ++ show (getStats minimized)
        putStrLn "\n\n\n"