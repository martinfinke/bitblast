module MatrixUtils where

import qualified Data.Matrix as M
import Data.List(sort)


dropMatrixRow :: Int -> M.Matrix a -> M.Matrix a
dropMatrixRow rowIndex matrix
    | rowIndex == 0 = bottom
    | rowIndex+1 == (M.nrows matrix) = top
    | otherwise = top M.<-> bottom
    where top = M.submatrix 1 rowIndex 1 (M.ncols matrix) matrix
          bottom = M.submatrix (rowIndex+2) (M.nrows matrix) 1 (M.ncols matrix) matrix

dropMatrixColumn :: Int -> M.Matrix a -> M.Matrix a
dropMatrixColumn columnIndex matrix
    | columnIndex == 0 = right
    | columnIndex+1 == (M.ncols matrix) = left
    | otherwise = left M.<|> right
    where left = M.submatrix 1 (M.nrows matrix) 1 columnIndex matrix
          right = M.submatrix 1 (M.nrows matrix) (columnIndex+2) (M.ncols matrix) matrix

dropMatrixRows :: [Int] -> M.Matrix a -> M.Matrix a
dropMatrixRows rowIndices matrix = foldr dropMatrixRow matrix (sort rowIndices)

dropMatrixColumns :: [Int] -> M.Matrix a -> M.Matrix a
dropMatrixColumns columnIndices matrix = foldr dropMatrixColumn matrix (sort columnIndices)