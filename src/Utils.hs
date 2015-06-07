module Utils where

import qualified Data.Set as Set

mapWithRest :: (a -> [a] -> b) -> [a] -> [b]
mapWithRest f xs =
    let withIndex = zip [(0::Int)..] xs
    in map (\(i,x) -> f x (map snd $ filter ((/=) i . fst) withIndex)) withIndex