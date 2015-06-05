{-# language FlexibleContexts #-}
module SatchmoInterface where

import Satchmo.Counting.Binary as C
import Satchmo.SAT.Mini
import Satchmo.Boolean
import Satchmo.Code

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(forM)
import CoinCBCInterface(columns)

import Qm


--toSatchmo :: MonadSAT m => [QmTerm] -> Map.Map BitVector [Int] -> m (x Boolean)
toSatchmo primes cols = do
    primeVars <- forM primes (const boolean)
    forM (Map.elems cols) (assertCover primeVars)
    C.atmost (length primes) primeVars -- TODO: Lower this to get a smaller cover.
    return $ decode primeVars

assertCover :: MonadSAT m => [Boolean] -> [Int] -> m ()
assertCover primeVars coveringPrimeIndices = do
    let coveringPrimeVars = [primeVars!!i | i <- coveringPrimeIndices]
    assertOr coveringPrimeVars

testOnes = map (getTerm . fromString) ["0000", "0001", "0010", "1000", "0011", "0101", "1010", "0111", "1110", "1111"]
testPrimes = Set.toAscList $ compute_primes (Set.fromList testOnes)
testColumns = columns testPrimes testOnes
