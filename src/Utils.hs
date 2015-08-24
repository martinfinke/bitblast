module Utils where

import qualified Data.Set as Set

import Control.Monad
import Control.Concurrent
import qualified System.Random as R
import Control.Monad.Random

indexed :: [Int] -> [a] -> [a]
indexed is list = foldr (\(i,e) rest -> if i `elem` is then e:rest else rest) [] $ zip [0..] list

-- Returns a list with exactly num elements, with no duplicates (after calling f).
uniqueRandom :: (R.Random a, Eq b, R.RandomGen g) => (a -> b) -> g -> Int -> (g, [b])
uniqueRandom f rand num = uniqueRandom' f rand num []
uniqueRandom' _ rand 0 accum = (rand,accum)
uniqueRandom' f rand num accum =
    let (i,newRand) = R.random rand
        i' = f i
    in if i' `elem` accum
        then uniqueRandom' f newRand num accum
        else uniqueRandom' f newRand (num-1) (i':accum)


uniqueRandomR :: (Ord a, R.RandomGen g) => (Int -> a) -> (Int,Int) -> Int -> Rand g [a]
uniqueRandomR f range amount = fmap Set.toList $ uniqueRandomR' f range amount Set.empty
uniqueRandomR' :: (Ord a, R.RandomGen g) => (Int -> a) -> (Int,Int) -> Int -> Set.Set a -> Rand g (Set.Set a)
uniqueRandomR' f range@(lo,hi) amount accum
    | lo > hi = error $ "uniqueRandomR: Invalid range: " ++ show lo ++ " > " ++ show hi
    | amount < 0 = error $ "uniqueRandomR: Invalid amount: " ++ show amount
    | amount > (hi - lo + 1) = error $ "uniqueRandomR: Expected amount is too high: " ++ show amount ++ " > " ++ show (hi - lo + 1)
    | Set.size accum == amount = return accum
    | otherwise = do
        item <- fmap f $ liftRand (R.randomR range)
        uniqueRandomR' f range amount $ Set.insert item accum

shuffleList :: R.RandomGen g => [a] -> Rand g [a]
shuffleList [] = return []
shuffleList list = do
    idx <- liftRand $ R.randomR (0,length list - 1)
    let rest = take idx list ++ drop (idx+1) list
    shuffledRest <- shuffleList rest
    return $ (list!!idx) : shuffledRest

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

combinationsNoMirror :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinationsNoMirror i ls =
    let i' = min i (length ls)
    in reverse $ combinationsNoMirror' i' ls

combinationsNoMirror' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinationsNoMirror' 0 _ = [[]]
combinationsNoMirror' i xs = 
    let forOne x xs' = map (x:) $ combinationsNoMirror' (i-1) xs'
        forAll = foldr (\_ (accum, (x:xs')) -> (forOne x xs':accum, xs')) ([], xs) xs
    in concat . fst $ forAll

oneFromEachSublist :: [[a]] -> [[a]]
oneFromEachSublist [] = [[]]
oneFromEachSublist (list:lists) =
    let rest = oneFromEachSublist lists
    in concatMap (\x -> map (x:) rest) list
