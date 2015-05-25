{-|
Quine-McCluskey two-level logic minimization method.

Copyright 2008, Robert Dick <dickrp@eecs.umich.edu> with improvements
from Pat Maupin <pmaupin@gmail.com>.
Ported from Python.

Routines to compute the optimal sum of products implementation from sets of don't-care terms, minterms, and maxterms.


Library usage example:
  import Qm(qm)
  minterms = qm [1, 2, 5] [] [0, 7]
-}
module Qm where

import Control.Exception(assert)
import Data.List(find)
import Data.Bits(shift)
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import Data.Word(Word8)
import Data.Bits((.&.))

type QmTermEl = Maybe Bool
newtype QmTerm = QmTerm (U.Vector QmTermEl)

zero, one, dash :: Word8
zero = 0
one = 1
dash = -1

listOr :: [[a]] -> [a]
listOr lists = case find (not . null) lists of
    Nothing -> []
    Just list -> list

setOr :: [Set.Set a] -> Set.Set a
setOr sets = case find (not . Set.null) sets of
    Nothing -> Set.empty
    Just set -> set

integralLogBase :: (Integral a, Fractional b) => Int -> a -> b
integralLogBase base number = realToFrac $ logBase (fromIntegral base) (fromIntegral number)

byteToBool :: Word8 -> Bool
byteToBool 0 = False
byteToBool _ = True

qm :: [Int] -> [Int] -> [Int] -> [B.ByteString]
qm [] [] _ = error "Must specify either (or both) ones and zeros"
qm ones zeros dc =
    let elts = maximum [maximum (listOr [ones, zeros, dc]),
                        maximum (listOr [zeros, dc, ones]),
                        maximum (listOr [dc, ones, zeros])] + 1
        numvars = ceiling $ integralLogBase 2 elts
        elts' = shift 1 numvars
        all' = Set.fromList [b2s i numvars | i <- [0..elts'-1]]
        ones' = Set.fromList [b2s i numvars | i <- ones]
        zeros' = Set.fromList [b2s i numvars | i <- zeros]
        dc' = Set.fromList [b2s i numvars | i <- dc]
        ones'' = setOr [ones', Set.difference (Set.difference all' zeros') dc']
        zeros'' = setOr [zeros', Set.difference (Set.difference all' ones'') dc']
        dc'' = setOr [dc', Set.difference (Set.difference all' ones'') zeros'']
        doAssert = assert $ Set.size dc'' + Set.size zeros'' + Set.size ones'' == elts' && Set.size (Set.unions [dc'', zeros'', ones'']) == elts'
        primes = doAssert $ compute_primes (Set.union ones'' dc'') numvars
    in  unate_cover primes ones''

unate_cover :: Set.Set B.ByteString -> Set.Set B.ByteString -> [B.ByteString]
unate_cover primes ones =
    let primes' = Set.elems primes
        cs = snd $ minimum [(bitcount (b2s cubesel (length primes')), cubesel) | cubesel <- [0..shift 1 (length primes')], is_full_cover (active_primes cubesel primes') ones]
    in  active_primes cs primes'

active_primes :: Int -> [B.ByteString] -> [B.ByteString]
active_primes cubesel primes = [prime | (used, prime) <- zip (B.foldr (\byte rest -> byteToBool byte : rest) [] $ b2s cubesel (length primes)) primes, used]

is_full_cover :: [B.ByteString] -> Set.Set B.ByteString -> Bool
is_full_cover all_primes ones = minimum (True : [maximum (False:[is_cover p o | p <- all_primes]) | o <- Set.toList ones])

is_cover :: B.ByteString -> B.ByteString -> Bool
is_cover prime term = minimum $ True : [p == dash || p == o | (p, o) <- B.zip prime term]

compute_primes :: Set.Set B.ByteString -> Int -> Set.Set B.ByteString
compute_primes cubes vars = primes
    where termsOrderedByNumRelevantBits = [Set.fromList [i | i <- Set.toList cubes, bitcount i == v] | v <- [0..vars]]
          (_, primes) = whileSigma (termsOrderedByNumRelevantBits, Set.empty)

whileSigma :: ([Set.Set B.ByteString], Set.Set B.ByteString) -> ([Set.Set B.ByteString], Set.Set B.ByteString)
whileSigma ([], primes) = ([], primes)
whileSigma (sigma, primes) =
    let (nsigma', redundant') = forC1C2 $ zip (init sigma) (tail sigma)
        primes' = Set.union primes $ Set.difference (Set.fromList [c | cubes <- sigma, c <- Set.toList cubes]) redundant'
        sigma' = nsigma'
    in whileSigma (sigma', primes')

forC1C2 :: [(Set.Set B.ByteString, Set.Set B.ByteString)] -> ([Set.Set B.ByteString], Set.Set B.ByteString)
forC1C2 [] = ([], Set.empty)
forC1C2 ((c1,c2):rest) =
    let (nc, redundant) = forAInC1BInC2 (Set.toList c1) (Set.toList c2)
        (oldNSigma, oldRedundant) = forC1C2 rest
    in  (nc:oldNSigma, Set.union oldRedundant redundant)

forAInC1BInC2 :: [B.ByteString] -> [B.ByteString] -> (Set.Set B.ByteString, Set.Set B.ByteString)
forAInC1BInC2 c1 c2 =
    let allAAndB = [(a,b) | a <- c1, b <- c2]
        (nc, redundant) = foldr addMerge (Set.empty, Set.empty) allAAndB
        addMerge (a,b) (nc', redundant') = case merge a b of
            Nothing -> (nc', redundant')
            Just m -> (Set.insert m nc', foldr Set.insert redundant' [a,b])
    in  (nc, redundant)

bitcount :: B.ByteString -> Int
bitcount s = B.count (read "1") s

b2s :: Int -> Int -> B.ByteString
b2s i vars =
    let is = [shift i (-k) | k <- [0..vars-1]] :: [Int]
        s = B.reverse $ B.pack $ foldr (\i' bytes -> ([read "0", read "1"]!!(i' .&. 1)) : bytes) [] is
    in  s

merge :: B.ByteString -> B.ByteString -> Maybe B.ByteString
merge i j =
    let (s,_) = mergeLoop (B.zip i j)
    in fmap B.pack s

mergeLoop :: [(Word8, Word8)] -> (Maybe [Word8], Int)
mergeLoop [] = (Just [], 0)
mergeLoop ((a,b):rest)
    | dif_cnt > 1 = (Nothing, dif_cnt)
    | (a == dash || b == dash) && a /= b = (Nothing, dif_cnt)
    | a /= b = (fmap (dash :) maybeS, dif_cnt  + 1)
    | otherwise = (fmap (a :) maybeS, dif_cnt)
    where (maybeS, dif_cnt) = mergeLoop rest
