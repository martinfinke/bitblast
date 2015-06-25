module Utils where

import qualified Data.Set as Set

parallelForM :: Int -> (a -> IO b) -> [a] -> IO [b]
parallelForM numThreads action elements = do

    return []

-- teile die Liste in Sub-Listen der Größe n
-- erstelle numThreads Threads. 
-- erstelle für jede Sub-Liste einen Thread, der:
-- 1. 

-- Eingabe: Eine sehr lange Liste und eine Funktion, die darüber zu machen ist. 