module UtilsSpec where

import SpecHelper
import Utils

spec :: Spec
spec = do
    describe "divideList" $ do
        it "returns one empty list if the input list is empty" $ do
            divideList 4 ([]::[Int]) `shouldBe` [[]]
        it "throws an exception if numSections is zero" $ do
            evaluate (divideList 0 [1..5]) `shouldThrow` anyException
        it "works if numSublists is greater than the original list length" $ do
            divideList 3 [1..2] `shouldBe` [[1], [2]]
        it "makes the last list longer if it doesn't fit evenly" $ do
            divideList 3 [1..10] `shouldBe` [[1,2,3],[4,5,6],[7,8,9,10]]
    describe "oneFromEachSublist" $ do
        it "picks one element from each sublist" $ do
            oneFromEachSublist [[1,2], [10,20], [65], [100,200]] `shouldBe` [
                    [1,10,65,100],
                    [1,10,65,200],
                    [1,20,65,100],
                    [1,20,65,200],
                    [2,10,65,100],
                    [2,10,65,200],
                    [2,20,65,100],
                    [2,20,65,200]
                    ]