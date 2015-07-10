{-# language TupleSections #-}
module TruthBasedCore where

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Applicative

import Satchmo.SAT.Mini
import Satchmo.Code
import qualified Satchmo.Boolean as B
import qualified Satchmo.Counting.Binary as C

import Data.Time.Clock(getCurrentTime)

newtype Lit = Lit Int
    deriving (Eq, Ord, Show)
newtype Clause = Clause [Lit]
    deriving (Eq, Ord, Show)
newtype CNF = CNF [Clause]
    deriving (Eq, Ord, Show)

type Assignment = [Bool]


lit :: Int -> Bool -> Lit
lit i True = Lit i
lit i False = Lit $ -i

variableNumbers :: [Int]
variableNumbers = [1..]

covers :: Clause -> Assignment  -> Bool
covers (Clause ls) assignment =
    let clause = map (\(i, b) -> lit i (not b)) $ zip variableNumbers assignment
    in Set.fromList ls `Set.isSubsetOf` Set.fromList clause

clauses :: Int -> [Clause]
clauses numVars =
    map (Clause . catMaybes) $ sequence $ do
        i <- take numVars variableNumbers
        return [Just $ lit i True, Just $ lit i False, Nothing]

assignments :: Int -> [Assignment]
assignments numVars = sequence $ replicate numVars [False, True]

data Table = Table [Assignment] [Clause] [(Bool, [Bool])]
    deriving(Eq, Show)

table :: Int -> (Assignment -> Bool) -> Int -> Table
table numVars f numExtraVars =
    let cls = clauses (numVars+numExtraVars) :: [Clause]
        xys = [x ++ y | x <- assignments numVars, y <- assignments numExtraVars] :: [Assignment]
        m = [(f (take numVars xy), [clause `covers` xy | clause <- cls]) | xy <- xys]
    in Table xys cls m

makeCnf :: Int -- ^ number of variables in the (original) formula
    -> (Assignment -> Bool) -- ^ original formula
    -> Int -- ^ number of allowed extra variables
    -> Int
    -> IO (Maybe CNF)
makeCnf numVars f numExtraVars maxNumLiterals = do
    let cls = clauses (numVars+numExtraVars)
    selection <- solve $ do
        ws <- forM (assignments (numVars+numExtraVars)) $ \w -> (w,) <$> B.boolean
        let w = M.fromList ws :: M.Map Assignment B.Boolean

        -- forall x: f(x) <-> exists y: w(x,y)
        forM_ (assignments numVars) $ \x -> do
            let ys = assignments numExtraVars
            z <- B.or $ map (\y -> w M.! (x++y)) ys
            f_x <- B.constant $ f x
            e <- B.equals2 f_x z
            B.assert [e]

        -- forall x,y: (not w(x,y)) <-> OR({p | p <- cls, p `covers` (x,y))})
        ps <- replicateM (length cls) B.boolean
        let p = M.fromList $ zip cls ps :: M.Map Clause B.Boolean
        forM_ (assignments $ numVars+numExtraVars) $ \xy -> do
            z <- B.or $ do
                (k,v) <- M.toList p
                guard (k `covers` xy)
                return v
            e <- B.equals2 (B.not $ w M.! xy) z
            B.assert [e]

        -- minimization
        ok <- C.atmost maxNumLiterals $ do
            ((Clause lits), p') <- M.toList p
            replicate (length lits) p'
        B.assert [ok]

        return $ decode ps
    return $ case selection of
        Nothing -> Nothing
        Just ps -> Just . CNF . map snd . filter fst . zip ps $ cls

optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
optimize numVars f numExtraVars = do
    maybeSolution <- opt (maxBound::Int)
    case maybeSolution of
        Nothing -> error "No solution."
        Just cnf -> return cnf
    where opt maxNumLiterals = do
            solution <- makeCnf numVars f numExtraVars maxNumLiterals
            solutionOrBetter <- case solution of
                Nothing -> return Nothing
                Just cnf@(CNF clauses) -> do
                    let numClauses = length clauses
                    let numLits = numLiterals cnf
                    putStr $ "Found solution with " ++ show numClauses ++ " clauses and " ++ show numLits ++ " literals."
                    now <- getCurrentTime
                    putStr $ " (" ++ show now ++ ")\n"
                    let improve = do
                            result <- opt (numLits-1)
                            if isJust result
                                then return result
                                else return solution
                    if numClauses > 0 then improve else return . Just $ CNF clauses
            return solutionOrBetter


numLiterals :: CNF -> Int
numLiterals (CNF clauses) = sum $ map (\(Clause lits) -> length lits) clauses