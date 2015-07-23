{-# language TupleSections #-}
module TruthBasedCore where

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.List(intercalate, elemIndices, sort, nub, sortBy)
import Data.Ord(comparing)
import Control.Concurrent
import Data.Time.Clock

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
    clauseProvider :: Int -> [Assignment] -> [Assignment] -> [Clause],
    removeIllegals :: Bool,
    gracePeriod :: Float,  -- ^ When a thread finds a solution, how long it should wait (as a fraction of the time it ran) before cancelling other threads which try to find better solutions
    numThreads :: Int
    }

defaultOptions :: Options
defaultOptions = Options {
    clauseProvider = \numVars _ _ -> clauses numVars,
    removeIllegals = False,
    gracePeriod = 0.5,
    numThreads = 10
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
table = tableWith defaultOptions
tableWith opts numVars f numExtraVars =
    let allClauses = callClauseProvider opts f numVars numExtraVars
        xys = [x ++ y | x <- assignments numVars, y <- assignments numExtraVars] :: [Assignment]
        m = [(f (take numVars xy), [clause `covers` xy | clause <- allClauses]) | xy <- xys]
        table = Table xys allClauses m
    in if removeIllegals opts then removeIllegalClauses numVars table else table

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
        in unlines $ header : divideLine : rowStrings ++ [tableInfo table]
        where 
              printAssignment = map (\b -> if b then '1' else '0')
              printClauseSet numVars cs = intercalate ", " (map (printClause numVars) $ Set.toAscList cs)
              printRow cellWidth (outputIsTrue, clauseCovers) = concatMap ((++ replicate (cellWidth-2) ' ') . printCell outputIsTrue) clauseCovers
              printCell outputIsTrue b
                | b = if outputIsTrue then "X " else "OK"
                | otherwise = "  "

tableInfo :: Table -> String
tableInfo (Table assignments clauses _) =
    unlines [
        (show . length) assignments ++ " assignments",
        (show . length) clauses ++ " clauses"
        ]

printClause :: Int -> Clause -> String
printClause numVars (Clause lits) = map (printLiteral lits) [1..numVars]
    where printLiteral lits i
                | lit i True `elem` lits = '1'
                | lit i False `elem` lits = '0'
                | otherwise = '-'

illegalClauses :: Int -> Table -> [Clause]
illegalClauses numVars (Table assignments clauses rows) =
    let numExtraVars = (length $ head assignments) - numVars
        coveredTrues clause = map snd $ filter (\((output,_), a) -> output && clause `covers` a) $ zip rows assignments :: [Assignment]
        byPrefix trues = foldr (\a m -> M.insertWith (++) (take numVars a) [a] m) M.empty trues :: M.Map Assignment [Assignment]
        addIfIllegal clause accum = if any ((==) (2^numExtraVars) . length) . map snd . M.toList . byPrefix . coveredTrues $ clause
                                    then clause:accum
                                    else accum
    in foldr addIfIllegal [] clauses

removeIllegalClauses :: Int -> Table -> Table
removeIllegalClauses numVars table@(Table assignments clauses rows) =
    let illegals = Set.fromList $ illegalClauses numVars table
        filteredClauses = Set.fromList clauses `Set.difference` illegals
    in Table assignments (Set.toList filteredClauses) rows

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
    where table = tableWith options numVars f numExtraVars
          (Table _ cls _) = if removeIllegals options then removeIllegalClauses numVars table else table
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
                    if numLits > 0 then improve else return . Just $ CNF clauses
            return solutionOrBetter

callClauseProvider :: Options -> (Assignment -> Bool) -> Int -> Int -> [Clause]
callClauseProvider options f numVars numExtraVars =
    let allAssignments = assignments (numVars+numExtraVars)
        ones = filter (f . take numVars) allAssignments
        zeros = filter (not . f . take numVars) allAssignments
    in (clauseProvider options) (numVars+numExtraVars) ones zeros

numLiterals :: CNF -> Int
numLiterals (CNF clauses) = sum $ map (\(Clause lits) -> length lits) clauses

data Result = Solution CNF | NoSolution | Cancelled

optimizeParallel :: Options
                 -> Int -- ^ The number of literals in the formula you already have (from Quine-McCluskey, or from before the system crashed). Pass (maxBound::Int) to be safe, but slow.
                 -> Int -- ^ Number of variables in the formula
                 -> (Assignment -> Bool) -- ^ The formula to find a minimal CNF for
                 -> Int -- ^ Number of extra variables
                 -> IO (Maybe CNF)
optimizeParallel options bestKnownValue numVars f numExtraVars =
    let table = tableWith options numVars f numExtraVars
        (Table _ cls _) = if removeIllegals options
                            then removeIllegalClauses numVars table
                            else table
        opt bestKnown = do
            putStrLn $ "Searching solution with < " ++ show bestKnown ++ " literals"
            let bounds = sort $ attempts (numThreads options) bestKnown
            mvars <- forM bounds (const newEmptyMVar)
            threadIds <- forM (zip [0..] bounds) $ \(i, maxNumLits) -> do
                forkIO $ do
                    startTime <- getCurrentTime
                    solution <- makeCnf numVars f numExtraVars cls maxNumLits
                    endTime <- getCurrentTime
                    let elapsedMicroseconds = fromIntegral . round $ diffUTCTime endTime startTime * 10^6
                    case solution of
                        Nothing -> do
                            putStrLn $ "Didn't find a solution with " ++ show maxNumLits ++ " literals"
                            putMVar (mvars!!i) NoSolution
                            -- There can't be a better solution, so cancel threads that are looking for one:
                            let lower = take i mvars
                            forM_ lower (flip putMVar Cancelled)
                        Just cnf ->do
                            -- We don't need worse solutions, so cancel the threads that are looking for one:
                            let higher = drop (i+1) mvars
                            forM_ higher (flip putMVar Cancelled)
                            -- Return our own result:
                            putStrLn $ "Found solution with " ++ show (numLiterals cnf) ++ " literals"
                            putMVar (mvars!!i) (Solution cnf)
                            -- Wait some time, then cancel threads that are looking for better solutions:
                            threadDelay $ round ((gracePeriod options)*elapsedMicroseconds)
                            let lower = take i mvars
                            putStrLn $ "Cancelling lower threads after grace period"
                            forM_ lower $ \mvar -> do
                                stillNothing <- isEmptyMVar mvar
                                guard stillNothing
                                putMVar mvar Cancelled
            results <- forM mvars readMVar
            putStrLn $ "Collected results for " ++ show bestKnown ++ " literals"
            solutions <- fmap catMaybes $ forM (zip results threadIds) $ \(result,threadId) -> case result of
                NoSolution -> return Nothing
                Solution cnf -> return $ Just cnf
                Cancelled -> do killThread threadId; return Nothing
            case sortBy (comparing numLiterals) solutions of
                [] -> return Nothing
                (sol:_) -> do
                    maybeBetter <- opt (numLiterals sol)
                    return $ case maybeBetter of
                        Nothing -> Just sol
                        Just better -> Just better

    in opt bestKnownValue
        
    
attempts :: Int -> Int -> [Int]
attempts num best
    | num == 0 = []
    | best <= 0 = []
    | otherwise = nub $ [floor $ low + i*step |  i <- [0..num'-2::Float]] ++ [best-1]
    where low = fromIntegral best / 2 :: Float
          high = fromIntegral $ best - 1
          num' = fromIntegral num
          range = (high - low)
          step = range / (num'-1)
