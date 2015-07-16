{-# language TupleSections #-}
module TruthBasedCore where

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.List(intercalate, elemIndices)

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

data Options = Options {
    clauseProvider :: Int -> [Assignment] -> [Assignment] -> [Clause]
    }

defaultOptions :: Options
defaultOptions = Options {
    clauseProvider = \numVars _ _ -> clauses numVars
    }

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
    deriving(Eq)

-- Example:     table 2 (\[a,b] -> a || (a==b)) 1
table :: Int -> (Assignment -> Bool) -> Int -> Table
table numVars f numExtraVars =
    let cls = clauses (numVars+numExtraVars) :: [Clause]
        xys = [x ++ y | x <- assignments numVars, y <- assignments numExtraVars] :: [Assignment]
        m = [(f (take numVars xy), [clause `covers` xy | clause <- cls]) | xy <- xys]
    in Table xys cls m

instance Show Table where
    show table@(Table assignments clauses rows) =
        let numVars = length $ head assignments
            sep = " | "
            headerPadding = replicate numVars ' ' ++ sep
            header = headerPadding ++ intercalate sep (map (printClause numVars) clauses)
            divideLine = replicate (length header) '-'
            assignmentStrings = map printAssignment assignments
            cellWidth = length sep + numVars
            matrixRows = map (printRow cellWidth) rows
            rowStrings = map (\(a,r) -> a ++ sep ++ r) $ zip assignmentStrings matrixRows
            --(nc,na,good) = redundancy table
            --neverCoverInfo = show (Set.size nc) ++ " clauses don't cover anything good:\n" ++ printClauseSet numVars nc
            --neverAllowedInfo = show (Set.size na) ++ " clauses cover too much:\n"  ++ printClauseSet numVars na
            --goodClausesInfo = show (Set.size good) ++ " clauses are candidates:\n" ++ printClauseSet numVars good
            --redundancyInfo = ["", neverCoverInfo, "", neverAllowedInfo, "", goodClausesInfo]
        in unlines $ header : divideLine : rowStrings
        where 
              printAssignment = map (\b -> if b then '1' else '0')
              printClauseSet numVars cs = intercalate ", " (map (printClause numVars) $ Set.toAscList cs)
              printRow cellWidth (outputIsTrue, clauseCovers) = concatMap ((++ replicate (cellWidth-2) ' ') . printCell outputIsTrue) clauseCovers
              printCell outputIsTrue b
                | b = if outputIsTrue then "X " else "OK"
                | otherwise = "  "

printClause :: Int -> Clause -> String
printClause numVars (Clause lits) = map (printLiteral lits) [1..numVars]
    where printLiteral lits i
                | lit i True `elem` lits = '1'
                | lit i False `elem` lits = '0'
                | otherwise = '-'

neverCover :: Table -> Set.Set Clause
neverCover (Table _ clauses rows) =
    let allClauses = Set.fromList clauses
    in Set.difference allClauses $ foldr containsAllowedCover Set.empty rows
    where containsAllowedCover (b, row) accum
            | b = accum
            | otherwise =
                let is = elemIndices True row
                in Set.union (Set.fromList $ indexed is clauses) accum

notAllowed :: Table -> Set.Set Clause
notAllowed (Table _ clauses rows) = foldr containsForbiddenCover Set.empty rows
    where containsForbiddenCover (b, row) accum
            | not b = accum
            | otherwise =
                let is = elemIndices True row
                in Set.union (Set.fromList $ indexed is clauses) accum

-- | Returns 1. clauses that don't cover anything, 2. clauses that cover too much, 3. the remainder, i.e. the clauses that should be used to look for a minimum cover.
redundancy :: Table -> (Set.Set Clause, Set.Set Clause, Set.Set Clause)
redundancy table@(Table _ clauses _) =
    let allClauses = Set.fromList clauses
        nc = neverCover table
        na = notAllowed table
    in (nc, na, (allClauses Set.\\ nc) Set.\\ na)

indexed :: [Int] -> [a] -> [a]
indexed is list = foldr (\(i,e) rest -> if i `elem` is then e:rest else rest) [] $ zip [0..] list

makeCnf :: Int -- ^ number of variables in the (original) formula
    -> (Assignment -> Bool) -- ^ original formula
    -> Int -- ^ number of allowed extra variables
    -> [Clause] -- ^ All clauses that should be candidates for the cover
    -> Int
    -> IO (Maybe CNF)
makeCnf numVars f numExtraVars cls maxNumLiterals = do
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
optimize = optimizeWith defaultOptions

optimizeWith options numVars f numExtraVars = do
    maybeSolution <- opt (maxBound::Int)
    case maybeSolution of
        Nothing -> error "No solution."
        Just cnf -> return cnf
    where allAssignments = assignments (numVars+numExtraVars)
          ones = filter (f . take numVars) allAssignments
          zeros = filter (not . f . take numVars) allAssignments
          cls = (clauseProvider options) (numVars+numExtraVars) ones zeros
          opt maxNumLiterals = do
            solution <- makeCnf numVars f numExtraVars cls maxNumLiterals
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