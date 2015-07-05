{-# language TupleSections #-}
module TruthBased where

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

covers :: Clause -> [Bool]  -> Bool
covers (Clause ls) assignment =
    let clause = map (\(i, b) -> lit i (not b)) $ zip [1..] assignment
    in Set.isSubsetOf (Set.fromList ls) (Set.fromList clause)

clauses :: Int -> [Clause]
clauses numVars =
    map (Clause . catMaybes) $ sequence $ do
        i <- [1..numVars]
        return [Just $ lit i True, Just $ lit i False, Nothing]

opt :: Int -- ^ number of variables in the (original) formula
    -> ([Bool] -> Bool) -- ^ original formula
    -> Int -- ^ number of allowed extra variables
    -> Int -- ^ number of clauses (reduce this to minimize)
    -> IO CNF
opt a f h c = do
    let cls = clauses (a+h)
    selection <- solve $ do
        ws <- forM (assignments (a+h)) $ \w -> (w,) <$> B.boolean
        let w = M.fromList ws :: M.Map [Bool] B.Boolean

        -- forall x: f(x) <-> exists y: w(x,y)
        forM_ (assignments a) $ \x -> do
            let ys = assignments h
            z <- B.or $ map (\y -> w M.! (x++y)) ys
            f_x <- B.constant $ f x
            e <- B.equals2 f_x z
            B.assert [e]

        -- forall x,y: (not w(x,y)) <-> OR({p | p <- cls, cover (x,y) p)})
        ps <- replicateM (length cls) B.boolean
        let p = M.fromList $ zip cls ps :: M.Map Clause B.Boolean
        forM_ (assignments $ a+h) $ \xy -> do
            z <- B.or $ do
                (k,v) <- M.toList p
                guard (k `covers` xy)
                return v
            e <- B.equals2 (B.not $ w M.! xy) z
            B.assert [e]

        -- minimization
        ok <- C.atmost c ps
        B.assert [ok]

        return $ decode ps
    case selection of
        Nothing -> error "Impossibru!"
        Just ps -> do
            let cnf = CNF $ map snd $ filter fst $ zip ps cls
            return cnf

testF [x,y,z] = x == (y && z)

testG [x] = not x

testFtest = opt 3 testF 1 3

testGtest = opt 1 testG 0 3

assignments :: Int -> [[Bool]]
assignments n = sequence $ replicate n [False, True]