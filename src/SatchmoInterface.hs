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
    C.atmost atMost primeVars
    return $ decode primeVars

assertCover :: MonadSAT m => [Boolean] -> [Int] -> m ()
assertCover primeVars coveringPrimeIndices = do
    let coveringPrimeVars = [primeVars!!i | i <- coveringPrimeIndices]
    assertOr coveringPrimeVars

runSatchmo :: [QmTerm] -> [BitVector] -> IO [QmTerm]
runSatchmo primes ones = do
    let cols = columns primes ones
    let base = makePrimeVars primes cols
    solution <- optimize base (length primes)
    return $ case solution of
        Nothing -> error "No solution."
        Just bools -> fromSatchmo primes bools

optimize :: SAT [Boolean] -> Int -> IO (Maybe [Bool])
optimize base maxNumPrimes = do
    let problem = withAtMost base maxNumPrimes
    solution <- solve problem
    solutionOrBetter <- case solution of
        Nothing -> return Nothing
        Just bools -> do
            let usedPrimes = length . filter id $ bools
            maybeBetterSolution <- optimize base (usedPrimes-1)
            return . Just $ maybe bools id maybeBetterSolution
    return solutionOrBetter



main = do
    primes <- runSatchmo [] []
    putStrLn $ show primes
