{-# language TupleSections #-}
module TruthBased3 where

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Applicative


import Satchmo.Counting.Binary as C
import Satchmo.SAT.Mini
import qualified Satchmo.Boolean as B
import Satchmo.Code


newtype Lit = Lit Int
    deriving (Eq, Ord, Show)
newtype Clause = Clause [Lit]
    deriving (Eq, Ord, Show)
newtype CNF = CNF [Clause]
    deriving (Eq, Ord, Show)


lit :: Int -> Bool -> Lit
lit i True = Lit i
lit i False = Lit $ -i

cover :: [Bool] -> Clause -> Bool
cover assignment (Clause ls) =
    let clause = map (\(i, b) -> lit i (not b)) $ zip [1..] assignment
    in Set.isSubsetOf (Set.fromList ls) (Set.fromList clause)

test1 = cover [False, True, False] (Clause $ map Lit [1, -2]) == True

clauses :: Int -> [Clause]
clauses n =
    map (Clause . catMaybes) $ sequence $ do
        i <- [1..n]
        return [Just $ lit i True, Just $ lit i False, Nothing]

opt :: Int -> ([Bool] -> Bool) -> Int -> Int -> IO CNF
opt a f h c = do
    let cls = clauses (a+h)
    selection <- solve $ do
        ws <- forM (assignments (a+h)) $ \w -> (w,) <$> B.boolean
        let w = M.fromList ws :: M.Map [Bool] B.Boolean

        -- forall x: f(x) <-> exists y: w(x, y)
        forM_ (assignments a) $ \x -> do
            let ys = assignments h
            z <- B.or $ map (\y -> w M.! (x++y)) ys
            f_x <- B.constant $ f x
            e <- B.equals2 f_x z
            B.assert [e]


        ps <- replicateM (length cls) B.boolean
        let p = M.fromList $ zip cls ps :: M.Map Clause B.Boolean
        forM_ (assignments $ a+h) $ \xy -> do
            z <- B.or $ do
                (k,v) <- M.toList p
                guard (cover xy k)
                return v
            e <- B.equals2 (w M.! xy) z
            B.assert [e]


        ok <- C.atmost c ps
        B.assert [ok]
        return $ decode ps
    case selection of
        Just ps -> do
            putStrLn "ps:"
            print (ps::[Bool])
            print $ filter fst $ zip ps cls

    return undefined

testF [x,y,z] = x == (y && z)

testG [x] = not x

testFtest = opt 3 testF 1 3


testGtest = opt 1 testG 0 3

assignments :: Int -> [[Bool]]
assignments n = sequence $ replicate n [False, True]