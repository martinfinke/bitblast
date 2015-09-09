module ArithmeticsModularSpec where

import SpecHelper
import ArithmeticsModular
import Formula(equisatGTE)
import Control.Monad
import Arithmetics

spec :: Spec
spec = do
    describe "bitVectors" $ do
        it "divides a variable list into bit vectors" $ do
            bitVectors 2 [0..7] `shouldBe` ([0,1], [2,3], [4,5], 6, 7, [])
            bitVectors 2 [0..10] `shouldBe` ([0,1], [2,3], [4,5], 6, 7, [8..10])
        it "throws an error if the variable list is too short" $ do
            evaluate (bitVectors 2 [0..6]) `shouldThrow` anyException

    describe "combine" $ do
        it "returns a circuit that is equisatGTE to a circuit that was created in one piece" $ do
            let bitGroups = [
                      (1,1)
                    , (1,2)
                    , (2,1)
                    , (2,2)
                    ]
            let check (lBits,hBits) =
                    let numBits = lBits + hBits
                        onePiece = nBitAddition Forbid numBits
                        low = summerModule lBits
                        high = summerModule hBits
                        comb = combine (lBits,hBits) low high
                        normalized = forbidOverflow numBits . noCarryIn numBits $ comb
                        result = normalized `equisatGTE` onePiece
                    in result `shouldBe` True
            mapM_ check bitGroups

