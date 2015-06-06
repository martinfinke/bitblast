{-|
The Quine-McCluskey Algorithm for Minimization of Boolean Functions.

Ported to Haskell from the Python qm Package (<http://pypi.python.org/pypi/qm>) and its improved version (including Petrick's method) by George Prekas (<http://github.com/prekageo/optistate>).
-}
module Qm(
          BitVector,
          QmTerm(..),
          getTerm,
          getMask,
          getMaskedTerm,
          fromString,
          printAsNumbers,
          qm,
          prepareInput,
          compute_primes,
          calculate_complexity,
          primeComplexity,
          prepareCovers,
          invertedNumvarsMask,
          minimize_complexity,
          bitcount,
          primeCoversOne,
          merge,
          unate_cover
          ) where


import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Bits as B
import Data.List(find)
import Data.Word(Word64)
import Control.Exception(assert)
import qualified Control.Monad.State.Lazy as State
import Control.Monad(forM_)
import Debug.Trace(traceShow)

-- | A fixed-width vector of 64 Bits.
type BitVector = Word64

-- | A min- or maxterm consisting of a tuple (variables, mask). The variables 'BitVector' contains the 'True'/'False' values of the variables. The mask 'BitVector' contains the dashes, i.e. for each 1 in the mask, the variable at that position can take any value.
newtype QmTerm = QmTerm (BitVector, BitVector) -- ^ Construct a 'QmTerm' out of a tuple (variables, mask).
    deriving(Ord, Show)

-- | Extract the variables 'BitVector'.
getTerm :: QmTerm -> BitVector
getTerm (QmTerm (bv,_)) = bv

-- | Extract the mask 'BitVector'.
getMask :: QmTerm -> BitVector
getMask (QmTerm (_,mask)) = mask

-- | Extract the variables 'BitVector', with all masked positions set to 0.
getMaskedTerm :: QmTerm -> BitVector
getMaskedTerm (QmTerm (term, mask)) = maskBitVector term mask

maskBitVector :: BitVector -> BitVector -> BitVector
maskBitVector bv mask = bv B..&. (B.complement mask)

instance Eq QmTerm where
    term1 == term2
        | getMask term1 /= getMask term2 = False
        | otherwise = getMaskedTerm term1 == getMaskedTerm term2

--instance Show QmTerm where
--    show (QmTerm (term, mask)) = dropWhile (== '0') $ map printBit $ reverse [0..B.finiteBitSize term - 1]
--        where printBit i = case B.testBit mask i of
--                True -> '-'
--                False -> if B.testBit term i then '1' else '0'

printAsNumbers :: QmTerm -> String
printAsNumbers (QmTerm (bv,mask)) = show (bv,mask)

-- | Reads a 'String' of the form \"01-0-\" and creates a 'QmTerm'. The \"-\" characters indicate masked positions.
fromString :: String -> QmTerm
fromString = QmTerm . fst . (foldr parse ((0,0), 0))
    where parse char ((term, mask), pos) = case char of
            '0' -> ((term, mask), succ pos)
            '1' -> ((B.setBit term pos, mask), succ pos)
            '-' -> ((term, B.setBit mask pos), succ pos)

-- | The Quine-McCluskey algorithm. Calculates the minimum cover of prime terms for a list of ones, zeros and \"don't care\" terms. All three can be non-empty, but at least either the ones or zeros must be non-empty. Based on that, the rest will be determined automatically.
qm :: [BitVector] -- ^ Ones, i.e. the set of min/maxterms that must be covered.
   -> [BitVector] -- ^ Zeros, i.e. the set of min/maxterms that must not be covered.
   -> [BitVector] -- ^ \"Don't-care\"-terms. These may be covered or not, whichever yields a smaller subset of primes.
   -> [QmTerm] -- ^ The minimum set of prime terms, covering all ones.
qm [] [] _ = error "Must specify either (or both) ones and zeros"
qm ones zeros dc =
    let (numvars, ones', dc') = prepareInput ones zeros dc
        primes = compute_primes (Set.union ones' dc')
    in Set.toAscList $ snd $ unate_cover numvars (Set.toList primes) (Set.toAscList ones')

prepareInput :: [BitVector] -> [BitVector] -> [BitVector] -> (Int, Set.Set BitVector, Set.Set BitVector)
prepareInput ones zeros dc =
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
    in doAssert (numvars, ones'', dc'')


-- | Calculates the (generally not minimum) set of prime terms covering a set of min/maxterms.
compute_primes :: Set.Set BitVector -- ^ Min/maxterms that must be covered.
               -> Set.Set QmTerm -- ^ The set of prime terms covering all min/maxterms.
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

-- | Counts the number of 1s (or 0s) in a 'BitVector'.
bitcount :: B.Bits a => Bool -- ^ 'True' to count 1s, 'False' to count 0s.
         -> a
         -> Int
bitcount True = B.popCount
bitcount False = bitcount True . B.complement

-- | Merges two 'QmTerm's into one, if allowed. The merge is allowed iff (1) the masks of the two 'QmTerm's are identical and (2) the two variable 'BitVector's differ in no more than one position. 
merge :: QmTerm
      -> QmTerm
      -> Maybe QmTerm -- ^ Merged 'QmTerm', or 'Nothing' if the merge isn't allowed.
merge i j
    | getMask i /= getMask j = Nothing -- Masks have to be the same
    | B.popCount y > 1 = Nothing -- There may be at most one difference between i and j
    | otherwise = Just $ QmTerm (getTerm i B..&. getTerm j, getMask i B..|. y)
    where y = getTerm i `B.xor` getTerm j -- All positions where i and j are different

type UnateCoverState = ([IntSet.IntSet], [IntSet.IntSet])

-- | Calculates the minimum subset of primes covering all min/maxterms, using Petrick's method (http:\/\/en.wikipedia.org\/wiki\/Petrick's_method).
unate_cover :: Int -- ^ Number of variables
            -> [QmTerm] -- ^ List of primes (generally not minimal)
            -> [BitVector] -- ^ Min/maxterms that must be covered
            -> (Int, Set.Set QmTerm) -- ^ The minimum subset of primes.
unate_cover numvars primes ones =
    let finalCovers = prepareCovers primes ones
    in minimize_complexity numvars primes finalCovers

prepareCovers :: [QmTerm] -> [BitVector] -> [IntSet.IntSet]
prepareCovers primes ones =
    let chart = [[i | (i,prime) <- zip [0..] primes, primeCoversOne prime one] | one <- ones] :: [[Int]]
        -- chart is the same as in Python.
        (initialCovers,chartRest) = if length chart > 0
            then (map IntSet.singleton (head chart), tail chart)
            else ([], [])
        (finalCovers,_) = flip State.execState (initialCovers, []) $ do
            forM_ chartRest $ \column -> do
                currentCovers <- fmap fst State.get
                forM_ currentCovers $ \cover ->
                    forM_ column $ \prime_index ->
                        State.modify $ updateNewCovers prime_index cover
                State.modify migrateNewCovers
    in finalCovers

primeCoversOne :: QmTerm -> BitVector -> Bool
primeCoversOne prime one = (one B..&. (B.complement $ getMask prime)) == getTerm prime

migrateNewCovers :: UnateCoverState -> UnateCoverState
migrateNewCovers (_, new_covers) = (new_covers, [])

updateNewCovers :: Int -> IntSet.IntSet -> UnateCoverState -> UnateCoverState
updateNewCovers prime_index cover (covers, new_covers) =
    let x = IntSet.insert prime_index cover
        new_covers' = filter (\new_cover -> not $ x `IntSet.isSubsetOf` new_cover) new_covers
        append = all (\new_cover -> not $ new_cover `IntSet.isProperSubsetOf` x) new_covers'
        new_covers'' = if append then x:new_covers' else new_covers'
    in (covers, new_covers'')

minimize_complexity :: Int -> [QmTerm] -> [IntSet.IntSet] -> (Int, Set.Set QmTerm)
minimize_complexity numvars primes covers =
    let mappedPrimes = IntMap.fromList $ zip [0..] primes
        primeSet = Set.fromList primes
        invertedNvMask = invertedNumvarsMask numvars
        forEachCover cover (min_complexity, currentBestCover) =
            let primes_in_cover = IntSet.foldr (\int rest -> Set.insert (mappedPrimes IntMap.! int) rest) Set.empty cover
                complexity = calculate_complexity invertedNvMask primes_in_cover
            in  if complexity < min_complexity
                then (complexity, primes_in_cover)
                else (min_complexity, currentBestCover)
    in foldr forEachCover (calculate_complexity invertedNvMask primeSet, primeSet) covers

-- | Counts the number of unmasked positions in each 'QmTerm', and returns the sum over all of them. So the result is the number of literals in the resulting CNF/DNF.
calculate_complexity :: BitVector -> Set.Set QmTerm -> Int
calculate_complexity invertedNvMask primes = sum $ map (primeComplexity invertedNvMask) (Set.toList primes)

primeComplexity :: BitVector -> QmTerm -> Int
primeComplexity invertedNvMask prime = (bitcount False . masked) prime
    where masked qmTerm = getMask qmTerm B..|. invertedNvMask

invertedNumvarsMask :: Int -> BitVector
invertedNumvarsMask numvars = B.complement $ (B.shiftL 1 numvars) - 1

setOr :: [Set.Set a] -> Set.Set a
setOr sets = case find (not . Set.null) sets of
    Nothing -> Set.empty
    Just set -> set

integralLogBase :: (Integral a, Fractional b) => Int -> a -> b
integralLogBase base number = realToFrac $ logBase (fromIntegral base) (fromIntegral number)