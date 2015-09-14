module ArithmeticsModularSpec where

import SpecHelper
import ArithmeticsModular
import Formula(equisatGTE)
import Control.Monad
import Arithmetics

spec :: Spec
spec = do
    describe "addBitVectors" $ do
        it "divides a variable list into bit vectors" $ do
            addBitVectors 2 [0..7] `shouldBe` ([0,1], [2,3], [4,5], 6, 7, [])
            addBitVectors 2 [0..10] `shouldBe` ([0,1], [2,3], [4,5], 6, 7, [8..10])
        it "throws an error if the variable list is too short" $ do
            evaluate (addBitVectors 2 [0..6]) `shouldThrow` anyException

    describe "mulBitVectors" $ do
        it "divides a variable list into bit vectors" $ do
            mulBitVectors 2 [0..7] `shouldBe` ([0,1], [2,3], [4,5,6,7], [])
            mulBitVectors 3 [0..15] `shouldBe` ([0,1,2], [3,4,5], [6,7,8,9,10,11], [12,13,14,15])
        it "throws an error if the variable list is too short" $ do
            evaluate (mulBitVectors 2 [0..6]) `shouldThrow` anyException

    -- Other, more expensive tests are in Main.testComb (not part of the normal test suite)
