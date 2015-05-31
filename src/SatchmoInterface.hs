module SatchmoInterface where

import Satchmo.Counting.Binary
import Satchmo.SAT.Mini
import qualified Data.Map as Map

import Qm

minimumCover :: [QmTerm] -> [BitVector] -> [QmTerm]
minimumCover primes ones =
    let indexedPrimes = zip [0..] primes
        columns = Map.fromList [(one,i) | one <- ones, (i,prime) <- indexedPrimes, primeCoversOne prime one]
    in undefined

columns :: [QmTerm] -> [BitVector] -> Map.Map BitVector [Int]
columns primes ones =
    let indexedPrimes = zip [0..] primes
        columns = Map.fromList [(one, [i | (i,prime) <- indexedPrimes, primeCoversOne prime one]) | one <- ones]
        uncoveredMinterms = Map.filter null columns
    in if Map.null uncoveredMinterms
        then columns
        else error $ "Uncovered minterms: " ++ show uncoveredMinterms