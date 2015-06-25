module Utils where

import qualified Data.Set as Set

import Control.Monad
import Control.Concurrent


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

parallelForM :: [a] -> (a -> IO b) -> IO [b]
parallelForM list action = do
    mvars <- forM list $ \x -> do
        m <- newEmptyMVar
        forkIO $ action x >>= putMVar m
        return m
    forM mvars takeMVar
