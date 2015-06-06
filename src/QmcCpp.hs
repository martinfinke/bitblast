{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module QmcCpp where

import Qm(BitVector,QmTerm(..))

import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as SV
import qualified Foreign.Marshal.Array as MA

foreign import ccall unsafe "../qmc-cpp/qmc/qmc.h answer_to_everything"
     c_answer_to_everything :: CInt -> CInt
     
foreign import ccall unsafe "../qmc-cpp/qmc/qmc.h compute_primes"
     c_compute_primes :: CSize -> Ptr CInt -> Ptr CInt



answer_to_everything :: Int -> Int
answer_to_everything x = fromIntegral (c_answer_to_everything (fromIntegral x))

qmcCppComputePrimes :: [BitVector] -> [QmTerm]
qmcCppComputePrimes ones = unsafePerformIO $ do
    let cOnes = map fromIntegral ones :: [CInt]
    withArray cOnes $ \onesPtr -> do
        let bitVectorPtr = c_compute_primes (fromIntegral $ length ones) onesPtr
        outputLength <- fmap fromIntegral . fmap head $ peekArray 1 bitVectorPtr
        (_:output) <- peekArray outputLength bitVectorPtr
        free bitVectorPtr
        return $ combine (map fromIntegral output)

combine :: [BitVector] -> [QmTerm]
combine [] = []
combine (_:[]) = error "not an even number of BitVectors."
combine (term:mask:rest) = QmTerm (term,mask) : combine rest