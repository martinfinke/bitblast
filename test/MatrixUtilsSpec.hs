module MatrixUtilsSpec where

import SpecHelper
import MatrixUtils
import qualified Data.Matrix as M



spec :: Spec
spec = do
    let testMatrix = M.fromLists [
            [0,1,0,0,0],
            [1,0,1,0,1],
            [0,1,0,1,1]
            ]
    describe "dropMatrixRow" $ do
        it "drops the first row correctly" $ do
            dropMatrixRow 0 testMatrix `shouldBe` M.fromLists [
                [1,0,1,0,1],
                [0,1,0,1,1]
                ]

        it "drops the last row correctly" $ do
            dropMatrixRow 2 testMatrix `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [1,0,1,0,1]
                ]

        it "drops a middle row correctly" $ do
            dropMatrixRow 1 testMatrix `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [0,1,0,1,1]
                ]

    describe "dropMatrixColumn" $ do
        it "drops the first column correctly" $ do
            dropMatrixColumn 0 testMatrix `shouldBe` M.fromLists [
                [1,0,0,0],
                [0,1,0,1],
                [1,0,1,1]
                ]

        it "drops the last column correctly" $ do
            dropMatrixColumn 4 testMatrix `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,1,0],
                [0,1,0,1]
                ]

        it "drops a middle column correctly" $ do
            dropMatrixColumn 2 testMatrix `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,0,1],
                [0,1,1,1]
                ]
            
    describe "dropMatrixColumns, dropMatrixRows" $ do
        it "does the same as dropMatrixColumn/Row if only one index is given" $ do
            dropMatrixColumns [4] testMatrix `shouldBe` dropMatrixColumn 4 testMatrix
            dropMatrixRows [2] testMatrix `shouldBe` dropMatrixRow 2 testMatrix

        it "drops the rightmost column (bottom-most row) first" $ do
            dropMatrixColumns [4,2] testMatrix `shouldBe` dropMatrixColumn 2 (dropMatrixColumn 4 testMatrix)
            dropMatrixRows [1,2] testMatrix `shouldBe` dropMatrixRow 1 (dropMatrixRow 2 testMatrix)

        it "does nothing without indices" $ do
            dropMatrixColumns [] testMatrix `shouldBe` testMatrix
            dropMatrixRows [] testMatrix `shouldBe` testMatrix

        it "doesn't care about the ordering of indices" $ do
            dropMatrixColumns [4,1,3,0] testMatrix `shouldBe` M.fromLists [
                    [0],
                    [1],
                    [0]
                    ]
            dropMatrixRows [2,0] testMatrix `shouldBe` M.fromLists [[1,0,1,0,1]]
            dropMatrixRows [0,2] testMatrix `shouldBe` M.fromLists [[1,0,1,0,1]]
