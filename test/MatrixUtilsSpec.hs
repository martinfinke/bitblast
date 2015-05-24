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
            dropMatrixRow testMatrix 0 `shouldBe` M.fromLists [
                [1,0,1,0,1],
                [0,1,0,1,1]
                ]

        it "drops the last row correctly" $ do
            dropMatrixRow testMatrix 2 `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [1,0,1,0,1]
                ]

        it "drops a middle row correctly" $ do
            dropMatrixRow testMatrix 1 `shouldBe` M.fromLists [
                [0,1,0,0,0],
                [0,1,0,1,1]
                ]

    describe "dropMatrixColumn" $ do
        it "drops the first column correctly" $ do
            dropMatrixColumn testMatrix 0 `shouldBe` M.fromLists [
                [1,0,0,0],
                [0,1,0,1],
                [1,0,1,1]
                ]

        it "drops the last column correctly" $ do
            dropMatrixColumn testMatrix 4 `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,1,0],
                [0,1,0,1]
                ]

        it "drops a middle column correctly" $ do
            dropMatrixColumn testMatrix 2 `shouldBe` M.fromLists [
                [0,1,0,0],
                [1,0,0,1],
                [0,1,1,1]
                ]
            
    describe "dropMatrixColumns" $ do
        it "does" $ do
            pending