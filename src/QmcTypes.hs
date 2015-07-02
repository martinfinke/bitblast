module QmcTypes(
          BitVector,
          QmTerm(..),
          getTerm,
          getMask,
          bitVectorToQmTerm,
          getMaskedTerm,
          fromString,
          printTerm,
          primeComplexity,
          invertedNumvarsMask,
          primeCoversOne,
          columns,
          bitcount
          ) where


import qualified Data.Bits as B
import qualified Data.Map as Map
import Data.Word(Word64)

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

bitVectorToQmTerm :: BitVector -> QmTerm
bitVectorToQmTerm bv = QmTerm (bv, 0)

-- | Extract the variables 'BitVector', with all masked positions set to 0.
getMaskedTerm :: QmTerm -> BitVector
getMaskedTerm (QmTerm (term, mask)) = maskBitVector term mask

maskBitVector :: BitVector -> BitVector -> BitVector
maskBitVector bv mask = bv B..&. (B.complement mask)

instance Eq QmTerm where
    term1 == term2
        | getMask term1 /= getMask term2 = False
        | otherwise = getMaskedTerm term1 == getMaskedTerm term2

printTerm :: Int -> QmTerm -> String
printTerm len qmTerm  = (replicate (len - (length $ printTerm' qmTerm)) '0') ++ printTerm' qmTerm ++ " 1"
    where printTerm' (QmTerm (term, mask)) = dropWhile (== '0') $ map (printBit term mask) $ reverse [0..B.finiteBitSize term - 1]
          printBit term mask i = case B.testBit mask i of
                True -> '-'
                False -> if B.testBit term i then '1' else '0'

-- | Reads a 'String' of the form \"01-0-\" and creates a 'QmTerm'. The \"-\" characters indicate masked positions.
fromString :: String -> QmTerm
fromString = QmTerm . fst . (foldr parse ((0,0), 0))
    where parse char ((term, mask), pos) = case char of
            '0' -> ((term, mask), succ pos)
            '1' -> ((B.setBit term pos, mask), succ pos)
            '-' -> ((term, B.setBit mask pos), succ pos)

-- | Counts the number of 1s (or 0s) in a 'BitVector'.
bitcount :: B.Bits a => Bool -- ^ 'True' to count 1s, 'False' to count 0s.
         -> a
         -> Int
bitcount True = B.popCount
bitcount False = bitcount True . B.complement

primeCoversOne :: QmTerm -> BitVector -> Bool
primeCoversOne prime one = (one B..&. (B.complement $ getMask prime)) == getTerm prime

primeComplexity :: BitVector -> QmTerm -> Int
primeComplexity invertedNvMask prime = (bitcount False . masked) prime
    where masked qmTerm = getMask qmTerm B..|. invertedNvMask

invertedNumvarsMask :: Int -> BitVector
invertedNumvarsMask numvars = B.complement $ (B.shiftL 1 numvars) - 1

columns :: [QmTerm] -> [BitVector] -> Map.Map BitVector [Int]
columns primes ones =
    let indexedPrimes = zip [0..] primes
        cols = Map.fromList [(one, [i | (i,prime) <- indexedPrimes, primeCoversOne prime one]) | one <- ones]
        uncoveredMinterms = Map.filter null cols
    in if Map.null uncoveredMinterms
        then cols
        else error $ "Uncovered minterms: " ++ show (Map.keys uncoveredMinterms)