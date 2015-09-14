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

    describe "combineAdd" $ do
        let finalize numBits = forbidOverflow numBits . noCarryIn numBits
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
                        comb = combineAdd (lBits,hBits) low high
                        result = finalize numBits comb `equisatGTE` onePiece
                    in result `shouldBe` True
            mapM_ check bitGroups

        it "works when combining two circuits that were combined from circuits" $ do
            let onePiece = nBitAddition Forbid 4
                oneBit = summerModule 1
                twoBit = combineAdd (1,1) oneBit oneBit
                comb = combineAdd (2,2) twoBit twoBit
            finalize 4 comb `equisatGTE` onePiece `shouldBe` True
            --pending
