{-# language FlexibleContexts #-}
module SatchmoInterface where

import Satchmo.Counting.Binary as C
import Satchmo.SAT.Mini
import Satchmo.Boolean
import Satchmo.Code

import qualified Data.Map as Map
import Control.Monad(forM)

import QmcTypes

fromSatchmo :: [QmTerm] -> [Bool] -> [QmTerm]
fromSatchmo primes bools =
    let zipped = zip bools primes
        active = map snd $ filter fst zipped
    in active

makePrimeVars :: (Decode m Boolean Bool, MonadSAT m) => [QmTerm] -> Map.Map BitVector [Int] -> m [Boolean]
makePrimeVars primes cols = do
    primeVars <- forM primes (const boolean)
    forM (Map.elems cols) (assertCover primeVars)
    return primeVars

withAtMost :: (Decode m Boolean Bool, MonadSAT m) => m [Boolean] -> Int -> m (m [Bool])
withAtMost base atMost = do
    primeVars <- base
    limitPrimeVars <- C.atmost atMost primeVars
    assert [limitPrimeVars]
    return $ decode primeVars

assertCover :: MonadSAT m => [Boolean] -> [Int] -> m ()
assertCover primeVars coveringPrimeIndices = do
    let coveringPrimeVars = [primeVars!!i | i <- coveringPrimeIndices]
    assertOr coveringPrimeVars

runSatchmo :: Int -> [QmTerm] -> [BitVector] -> IO [QmTerm]
runSatchmo numVars primes ones = do
    let cols = columns primes ones
    let base = makePrimeVars primes cols
    solution <- optimize base (length primes)
    return $ case solution of
        Nothing -> error "No solution."
        Just bools -> fromSatchmo primes bools

optimize :: SAT [Boolean] -> Int -> IO (Maybe [Bool])
optimize base maxNumPrimes = do
    let problem = withAtMost base maxNumPrimes
    solution <- solveSilently problem
    solutionOrBetter <- case solution of
        Nothing -> do
            --putStrLn $ "Didn't find a solution with " ++ show maxNumPrimes ++ " primes."
            return Nothing
        Just bools -> do
            let usedPrimes = length . filter id $ bools
            --putStrLn $ "Found solution with " ++ show usedPrimes ++ " primes."
            let improve = do
                    --putStrLn $ "Trying with " ++ show (usedPrimes-1) ++ " primes."
                    result <- optimize base (usedPrimes-1)
                    return $ case result of
                        Nothing -> solution
                        Just _ -> result
            if usedPrimes > 0 then improve else return (Just bools)
    return solutionOrBetter
