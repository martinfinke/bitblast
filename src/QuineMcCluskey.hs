module QuineMcCluskey where

import Formula

numRelevantLiterals :: Formula -> Int
numRelevantLiterals formula
    | isMinterm formula = let (And literals) = formula in countPositive literals
    | isMaxterm formula = let (Or literals) = formula in countNegative literals
    | otherwise = error $ "Not a min- or maxterm: " ++ show formula
    where countPositive = length . filter isPositiveLiteral
          countNegative = length . filter (not . isPositiveLiteral)