module MatrixUtils where

import qualified Data.Matrix as M


dropMatrixRow :: M.Matrix a -> Int -> M.Matrix a
dropMatrixRow matrix rowIndex
    | rowIndex == 0 = bottom
    | rowIndex+1 == (M.nrows matrix) = top
    | otherwise = top M.<-> bottom
    where top = M.submatrix 1 rowIndex 1 (M.ncols matrix) matrix
          bottom = M.submatrix (rowIndex+2) (M.nrows matrix) 1 (M.ncols matrix) matrix

dropMatrixColumn :: M.Matrix a -> Int -> M.Matrix a
dropMatrixColumn matrix columnIndex
    | columnIndex == 0 = right
    | columnIndex+1 == (M.ncols matrix) = left
    | otherwise = left M.<|> right
    where left = M.submatrix 1 (M.nrows matrix) 1 columnIndex matrix
          right = M.submatrix 1 (M.nrows matrix) (columnIndex+2) (M.ncols matrix) matrix
