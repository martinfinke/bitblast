module Qm2 where


import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Bits as B
import Data.List(find)
import Data.Word
import Control.Exception(assert)
import qualified Control.Monad.State.Strict as State
import Control.Monad


type BitVector = Word64
newtype QmTerm = QmTerm (BitVector, BitVector)
    deriving(Ord)

getTerm :: QmTerm -> BitVector
getTerm (QmTerm (bv,_)) = bv

getMask :: QmTerm -> BitVector
getMask (QmTerm (_,mask)) = mask

getMaskedTerm :: QmTerm -> BitVector
getMaskedTerm (QmTerm (term, mask)) = maskBitVector term mask

maskBitVector :: BitVector -> BitVector -> BitVector
maskBitVector bv mask = bv B..&. (B.complement mask)

instance Eq QmTerm where
    term1 == term2
        | getMask term1 /= getMask term2 = False
        | otherwise = getMaskedTerm term1 == getMaskedTerm term2

instance Show QmTerm where
    show (QmTerm (term, mask)) = map printBit $ reverse [0..B.finiteBitSize term - 1]
        where printBit i = case B.testBit mask i of
                True -> '-'
                False -> if B.testBit term i then '1' else '0'

fromString :: String -> QmTerm
fromString = QmTerm . fst . (foldr parse ((0,0), 0))
    where parse char ((term, mask), pos) = case char of
            '0' -> ((term, mask), succ pos)
            '1' -> ((B.setBit term pos, mask), succ pos)
            '-' -> ((term, B.setBit mask pos), succ pos)

compute_primes :: Set.Set BitVector -> Set.Set QmTerm
compute_primes cubes =
    let sigma = Set.foldr insert IntMap.empty cubes
        sigmaAsList = map snd (IntMap.toAscList sigma) :: [Set.Set QmTerm]
        insert cube sigma' = IntMap.insertWith Set.union (bitcount True cube) (Set.singleton $ QmTerm (cube, 0::BitVector)) sigma'
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

merge :: QmTerm -> QmTerm -> Maybe QmTerm
merge i j
    | getMask i /= getMask j = Nothing -- Masks have to be the same
    | B.popCount y > 1 = Nothing -- There may be at most one difference between i and j
    | otherwise = Just $ QmTerm (getTerm i B..&. getTerm j, getMask i B..|. y)
    where y = getTerm i `B.xor` getTerm j -- All positions where i and j are different

type UnateCoverState = ([IntSet.IntSet], [IntSet.IntSet])

unate_cover :: [QmTerm] -> [BitVector] -> (Int, Set.Set QmTerm)
unate_cover primes ones =
    let chart = [[i | (i,prime) <- zip [0..] primes, primeCoversOne prime one] | one <- ones] :: [[Int]]
        (covers,chartRest) = if length chart > 0
            then (map IntSet.singleton (head chart), tail chart)
            else ([], [])
        (covers',_) = flip State.execState (covers, []) $ do
            forM_ chartRest $ \column -> do
                covers <- fmap fst State.get
                forM_ covers $ \cover ->
                    forM_ column $ \prime_index ->
                        State.modify $ updateNewCovers prime_index cover
                State.modify migrateNewCovers

    in minimize_complexity primes covers'

primeCoversOne :: QmTerm -> BitVector -> Bool
primeCoversOne prime one = QmTerm (one, getMask prime) == prime

migrateNewCovers :: UnateCoverState -> UnateCoverState
migrateNewCovers (_, new_covers) = (new_covers, [])

updateNewCovers :: Int -> IntSet.IntSet -> UnateCoverState -> UnateCoverState
updateNewCovers prime_index cover (covers, new_covers) =
    let x = IntSet.insert prime_index cover
        new_covers' = filter (`IntSet.isProperSubsetOf` x) new_covers
        append = all (x `IntSet.isSubsetOf`) new_covers'
    in (covers, if append then x:new_covers' else new_covers')

minimize_complexity :: [QmTerm] -> [IntSet.IntSet] -> (Int, Set.Set QmTerm)
minimize_complexity primes covers =
    let mappedPrimes = IntMap.fromList $ zip [0..] primes
        primeSet = Set.fromList primes
        forEachCover cover (min_complexity, result) =
            let primes_in_cover = IntSet.foldr (\int rest -> Set.insert (mappedPrimes IntMap.! int) rest) Set.empty cover
                complexity = calculate_complexity primes_in_cover
            in if complexity <= min_complexity then (complexity, primes_in_cover) else (min_complexity, result)
    in foldr forEachCover (calculate_complexity primeSet, primeSet) covers

-- | Counts the number of unmasked positions in each 'QmTerm', and returns the sum over all of them. So the result is the number of literals in the resulting CNF/DNF.
calculate_complexity :: Set.Set QmTerm -> Int
calculate_complexity primes = sum $ Set.map (bitcount False . getMask) primes


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

qm :: [BitVector] -> [BitVector] -> [BitVector] -> [QmTerm]
qm [] [] _ = error "Must specify either (or both) ones and zeros"
qm ones zeros dc =
    let 
        ones' = Set.fromList ones
        zeros' = Set.fromList zeros
        dc' = Set.fromList dc
        elts = maximum [Set.findMax (setOr [ones', zeros', dc']),
                        Set.findMax (setOr [zeros', dc', ones']),
                        Set.findMax (setOr [dc', ones', zeros'])] + 1
        numvars = ceiling $ integralLogBase 2 elts :: Int
        elts' = B.shift 1 numvars :: Int
        all' = Set.fromList $ map fromIntegral [0..elts'-1]
        ones'' = setOr [ones', Set.difference (Set.difference all' zeros') dc']
        zeros'' = setOr [zeros', Set.difference (Set.difference all' ones'') dc']
        dc'' = setOr [dc', Set.difference (Set.difference all' ones'') zeros'']
        doAssert = assert $ Set.size dc'' + Set.size zeros'' + Set.size ones'' == elts'
                         && Set.size (Set.unions [dc'', zeros'', ones'']) == elts'
        primes = doAssert $ compute_primes (Set.union ones'' dc'')
    in Set.toAscList $ snd $ unate_cover (Set.toAscList primes) (Set.toAscList ones'')

