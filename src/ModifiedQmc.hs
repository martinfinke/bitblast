module ModifiedQmc where

import QmcTypes
import qualified Data.Set as Set
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Bits as B

data MustOrMay = Must QmTerm | May QmTerm
    deriving(Eq, Ord)

extractQmTerm :: MustOrMay -> QmTerm
extractQmTerm (Must qmTerm) = qmTerm
extractQmTerm (May qmTerm) = qmTerm

isMust :: MustOrMay -> Bool
isMust (Must _) = True
isMust _ = False

modifiedQmc :: [BitVector] -> [BitVector] -> [QmTerm]
modifiedQmc [] [] = []
modifiedQmc mustVectors mayVectors =
    let mustTerms = map (Must . bitVectorToQmTerm) mustVectors
        mayTerms = map (May . bitVectorToQmTerm) mayVectors
        both = Set.fromList (mustTerms ++ mayTerms)
        sigma = Set.foldr insert IntMap.empty both
        sigmaAsList = map snd (IntMap.toAscList sigma)
        insert term sigma' = IntMap.insertWith Set.union (bitcount True . getTerm . extractQmTerm $ term) (Set.singleton term) sigma'
        (_, primes) = whileSigma (sigmaAsList, Set.empty)
    in map extractQmTerm $ Set.toAscList primes

whileSigma :: ([Set.Set MustOrMay], Set.Set MustOrMay) -> ([Set.Set MustOrMay], Set.Set MustOrMay)
whileSigma ([], primes) = ([], primes)
whileSigma (sigma, primes) =
    let (nsigma', redundant') = forC1C2 $ zip (init sigma) (tail sigma)
        primes' = Set.union primes $ Set.difference (Set.fromList [c | cubes <- sigma, c <- Set.toList cubes]) redundant'
        sigma' = nsigma'
    in whileSigma (sigma', primes')

forC1C2 :: [(Set.Set MustOrMay, Set.Set MustOrMay)] -> ([Set.Set MustOrMay], Set.Set MustOrMay)
forC1C2 [] = ([], Set.empty)
forC1C2 ((c1,c2):rest) =
    let (nc, redundant) = forAInC1BInC2 (Set.toList c1) (Set.toList c2)
        (oldNSigma, oldRedundant) = forC1C2 rest
    in  (nc:oldNSigma, Set.union oldRedundant redundant)

-- TODO: Does this terminate under all circumstances?
forAInC1BInC2 :: [MustOrMay] -> [MustOrMay] -> (Set.Set MustOrMay, Set.Set MustOrMay)
forAInC1BInC2 c1 c2 =
    let allAAndB = [(a,b) | a <- c1, b <- c2]
        (nc, redundant) = foldr addMerge (Set.empty, Set.empty) allAAndB
        addMerge (a,b) (nc', redundant') = case merge a b of
            NotAllowed -> (nc', redundant')
            KeepOperands m -> (Set.insert m nc', redundant')
            DiscardOperands m -> (Set.insert m nc', foldr Set.insert redundant' [a,b])
    in  (nc, redundant)

data MergeResult = NotAllowed
                 | KeepOperands MustOrMay
                 | DiscardOperands MustOrMay

merge :: MustOrMay
      -> MustOrMay
      -> MergeResult
merge i j
    | mask i /= mask j = NotAllowed -- Masks have to be the same
    | B.popCount y > 1 = NotAllowed -- There may be at most one difference between i and j
    | otherwise = keepOrDiscard $ QmTerm (term i B..&. term j, mask i B..|. y)
    where y = term i `B.xor` term j -- All positions where i and j are different
          term = getTerm . extractQmTerm
          mask = getMask . extractQmTerm
          keepOrDiscard = if isMust i && isMust j then DiscardOperands . Must else KeepOperands . May
