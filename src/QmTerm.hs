module QmTerm where

import qualified Data.Vector.Unboxed as U
import Data.Bits((.&.), shift)
import UnboxMaybe



type QmTermEl = Maybe Bool
newtype QmTerm = QmTerm (U.Vector QmTermEl)
    deriving (Eq, Ord)

instance Show QmTerm where
    show (QmTerm vector) = map showMaybeBool $ U.toList vector
        where showMaybeBool maybeBool = case maybeBool of
                Just True -> '1'
                Just False -> '0'
                Nothing -> '-'

fromString :: String -> QmTerm
fromString = QmTerm . U.fromList . map convert
    where convert c = case c of
            '0' -> zero
            '1' -> one
            '-' -> dash

zero, one, dash :: QmTermEl
zero = Just False
one = Just True
dash = Nothing


bitcount :: QmTerm -> Int
bitcount (QmTerm s) = U.length $ U.elemIndices (Just True) s

b2s :: Int -> Int -> QmTerm
b2s i vars =
    let is = [shift i (-k) | k <- [0..vars-1]] :: [Int]
        s' = reverse $ map (\i' -> ([zero, one]!!(i' .&. 1))) is
    in  QmTerm (U.fromList s')

s2b :: QmTerm -> Int
s2b (QmTerm vec) = U.sum $ U.imap power vec
    where numVars = U.length vec
          power index termEl = case termEl of
            Just True -> 2^(numVars-1-index)
            _ -> 0


merge :: QmTerm -> QmTerm -> Maybe QmTerm
merge (QmTerm i) (QmTerm j) =
    let (s,_) = mergeLoop (U.toList $ U.zip i j)
    in fmap (QmTerm . U.fromList) s

mergeLoop :: [(QmTermEl, QmTermEl)] -> (Maybe [QmTermEl], Int)
mergeLoop [] = (Just [], 0)
mergeLoop ((a,b):rest)
    | dif_cnt > 1 = (Nothing, dif_cnt)
    | (a == dash || b == dash) && a /= b = (Nothing, dif_cnt)
    | a /= b = (fmap (dash :) maybeS, dif_cnt  + 1)
    | otherwise = (fmap (a :) maybeS, dif_cnt)
    where (maybeS, dif_cnt) = mergeLoop rest
