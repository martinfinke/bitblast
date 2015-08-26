module UtilsSpec where

import SpecHelper
import Utils
import qualified System.Random as R
import Control.Monad
import Control.Monad.Random
import Data.List

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

    describe "shuffleList" $ do
        it "always preserves the length" $ do
            property $ \(IntList list) -> do
                shuffled <- evalRandIO $ shuffleList list
                length shuffled `shouldBe` length list
        it "never adds or removes elements" $ do
            property $ \(IntList list) -> do
                shuffled <- evalRandIO $ shuffleList list
                sort shuffled `shouldBe` sort list

    describe "uniqueRandomR" $ do
        let safeProp f = property $ \(OneHundredOrLess a) (OneHundredOrLess b) (TenOrLess amount) -> do
                let lo = min a b
                let hi = max a b
                when (amount <= hi - lo + 1) $ do
                    list <- evalRandIO $ uniqueRandomR id (lo,hi) amount
                    f list lo hi amount
        it "always returns a list of the desired length, if it is possible within the given range" $ safeProp $ \list _ _ amount -> do
            length list `shouldBe` amount
        it "never contains duplicates" $ safeProp $ \list _ _ _ -> do
            nub list `shouldBe` list

    describe "parallelForM" $ do
        it "works when the task list is empty" $ do
            result <- parallelForM 4 []
            result `shouldBe` ([]::[Int])
        it "always returns the right results in the right order" $ do
            property $ \(TenOrLess i) (TenOrLess numTasks) -> do
                let numThreads = max 1 i
                let tasks = map return [0..numTasks-1]
                result <- parallelForM numThreads tasks
                result `shouldBe` [0..numTasks-1]

    describe "paddedResize" $ do
        it "works correctly on lists both longer and shorter than the length we want" $ do
            paddedResize '-' 10 "012345" `shouldBe` "012345----"
            paddedResize '-' 3 "012345" `shouldBe` "012"
            paddedResizeWith id 10 [0,1,2,3,4,5] `shouldBe` [0,1,2,3,4,5,6,7,8,9]
            paddedResizeWith id 1 [0,1,2,3,4,5] `shouldBe` [0]
            paddedResizeWith id 0 [0,1,2,3,4,5] `shouldBe`  []
