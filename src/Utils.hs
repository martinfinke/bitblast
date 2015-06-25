module Utils where

import qualified Data.Set as Set


divideList :: Int -> [a] -> [[a]]
divideList numSublists list =
    let sublistLength = max 1 $ length list `div` numSublists
        helper list' =
            let current = take sublistLength list'
                rest = drop sublistLength list'
            in case rest of
                [] -> [current]
                nonEmpty -> if length nonEmpty >= sublistLength
                    then current : helper nonEmpty
                    else [current ++ nonEmpty]
    in helper list