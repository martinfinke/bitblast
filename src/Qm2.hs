module Qm2 where


import qualified Data.Set as Set
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Bits as B
import UnboxMaybe
import Debug.Trace(traceShow)
import Data.Word


type BitVector = Word64
newtype QmTerm = QmTerm (BitVector, BitVector)
    deriving(Ord)

getTerm :: QmTerm -> BitVector
getTerm (QmTerm (bv,_)) = bv

instance Eq QmTerm where
    (QmTerm (term1, mask1)) == (QmTerm (term2, mask2))
        | mask1 /= mask2 = False
        | otherwise = intersectionWithMask == 0
            where termDiff = term1 `B.xor` term2
                  intersectionWithMask = termDiff B..&. (B.complement mask1)


instance Show QmTerm where
    show (QmTerm (term, mask)) = map printBit $ reverse [0..B.finiteBitSize term - 1]
        where printBit i = case B.testBit mask i of
                True -> '-'
                False -> if B.testBit term i then '1' else '0'

fromString :: String -> QmTerm
fromString = QmTerm . fst . (foldr parse ((0,0), 0))
    where parse char ((term, mask), pos) = traceShow pos $ case char of
            '0' -> ((term, mask), pos+1)
            '1' -> ((B.setBit term pos, mask), pos+1)
            '-' -> ((term, B.setBit mask pos), pos+1)

compute_primes :: Bool -> Set.Set BitVector -> Int -> Set.Set QmTerm
compute_primes cnfMode cubes vars =
    let sigma = Set.foldr insert IntMap.empty cubes
        sigmaAsList = map snd (IntMap.toAscList sigma) :: [Set.Set QmTerm]
        insert cube sigma' = IntMap.insertWith Set.union (bitcount (not cnfMode) cube) (Set.singleton $ QmTerm (cube, 0::BitVector)) sigma'
        (_, primes) = whileSigma (sigmaAsList, Set.empty)
    in primes

whileSigma :: ([Set.Set QmTerm], Set.Set QmTerm) -> ([Set.Set QmTerm], Set.Set QmTerm)
whileSigma ([], primes) = ([], primes)
whileSigma (sigma, primes) =
    let (nsigma', redundant') = forC1C2 $ zip (init sigma) (tail sigma)
        primes' = Set.union primes $ Set.difference (Set.fromList [c | cubes <- sigma, c <- Set.toList cubes]) redundant'
        sigma' = nsigma'
    in whileSigma (sigma', primes')

forC1C2 :: [(Set.Set QmTerm, Set.Set QmTerm)] -> ([Set.Set QmTerm], Set.Set QmTerm)
forC1C2 [] = ([], Set.empty)
forC1C2 ((c1,c2):rest) =
    let (nc, redundant) = forAInC1BInC2 (Set.toList c1) (Set.toList c2)
        (oldNSigma, oldRedundant) = forC1C2 rest
    in  (nc:oldNSigma, Set.union oldRedundant redundant)

forAInC1BInC2 :: [QmTerm] -> [QmTerm] -> (Set.Set QmTerm, Set.Set QmTerm)
forAInC1BInC2 c1 c2 =
    let allAAndB = [(a,b) | a <- c1, b <- c2]
        (nc, redundant) = foldr addMerge (Set.empty, Set.empty) allAAndB
        addMerge (a,b) (nc', redundant') = case merge a b of
            Nothing -> (nc', redundant')
            Just m -> (Set.insert m nc', foldr Set.insert redundant' [a,b])
    in  (nc, redundant)

bitcount :: B.Bits a => Bool -- ^ 'True' to count 1s, 'False' to count 0s.
         -> a
         -> Int
bitcount True = B.popCount
bitcount False = bitcount True . B.complement

isPowerOfTwoOrZero :: BitVector -> Bool
isPowerOfTwoOrZero x = B.popCount x <= 1

merge :: QmTerm -> QmTerm -> Maybe QmTerm
merge (QmTerm i) (QmTerm j)
    | snd i /= snd j = Nothing -- Dashes have to be the same
    | not (isPowerOfTwoOrZero y) = Nothing -- y may contain at most one set bit (one difference between i and j)
    | otherwise = Just $ QmTerm (fst i B..&. fst j, snd i B..|. y)
    where y = (fst i) `B.xor` (fst j) -- All positions where i and j are different

