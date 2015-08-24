{-# language FlexibleContexts #-}
module SatchmoInterface where

import Satchmo.Counting.Binary as C
import qualified Satchmo.Boolean as B
import Satchmo.Boolean hiding(not)
import Satchmo.SAT.Mini
import Satchmo.Code
import Formula
import NormalForm

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Control.Monad(forM, forM_, replicateM)

import QmcTypes

fromSatchmo :: [QmTerm] -> [Bool] -> [QmTerm]
fromSatchmo primes bools =
    let zipped = zip bools primes
        active = map snd $ filter fst zipped
    in active

makePrimeVars :: (Decode m Boolean Bool, MonadSAT m) => [QmTerm] -> Map.Map BitVector [Int] -> m [Boolean]
makePrimeVars primes cols = do
    primeVars <- forM primes (const boolean)
    forM (Map.elems cols) (assertCover primeVars)
    return primeVars

withAtMost :: (Decode m Boolean Bool, MonadSAT m) => m [Boolean] -> Int -> m (m [Bool])
withAtMost base atMost = do
    primeVars <- base
    limitPrimeVars <- C.atmost atMost primeVars
    assert [limitPrimeVars]
    return $ decode primeVars

assertCover :: MonadSAT m => [Boolean] -> [Int] -> m ()
assertCover primeVars coveringPrimeIndices = do
    let coveringPrimeVars = [primeVars!!i | i <- coveringPrimeIndices]
    assertOr coveringPrimeVars

runSatchmo :: Int -> [QmTerm] -> [BitVector] -> IO [QmTerm]
runSatchmo numVars primes ones = do
    let cols = columns primes ones
    let base = makePrimeVars primes cols
    solution <- optimize base (length primes)
    return $ case solution of
        Nothing -> error "No solution."
        Just bools -> fromSatchmo primes bools

optimize :: SAT [Boolean] -> Int -> IO (Maybe [Bool])
optimize base maxNumPrimes = do
    let problem = withAtMost base maxNumPrimes
    solution <- solveSilently problem
    solutionOrBetter <- case solution of
        Nothing -> do
            --putStrLn $ "Didn't find a solution with " ++ show maxNumPrimes ++ " primes."
            return Nothing
        Just bools -> do
            let usedPrimes = length . filter id $ bools
            --putStrLn $ "Found solution with " ++ show usedPrimes ++ " primes."
            let improve = do
                    --putStrLn $ "Trying with " ++ show (usedPrimes-1) ++ " primes."
                    result <- optimize base (usedPrimes-1)
                    return $ case result of
                        Nothing -> solution
                        Just _ -> result
            if usedPrimes > 0 then improve else return (Just bools)
    return solutionOrBetter


-- | Requires: import qualified Satchmo.Boolean as B
cnfToSatchmo :: String -> Formula -> Int -> String
cnfToSatchmo name f numBits
    | not (isCnf f) = error $ "cnfToSatchmo: Formula is not a CNF:\n" ++ show f
    | otherwise =
        let functionName = name ++ show numBits
            typeSignature = functionName ++ " :: B.MonadSAT m => [B.Boolean] -> [B.Boolean] -> m [B.Boolean]"
            header = functionName ++ " xs ys = do"
            indent amount str = replicate amount ' ' ++ str

            clauses = normalFormChildren f
            allVars = Set.toAscList . variableSet $ f
            vars = take (3*numBits) allVars
            xs' = reverse . take numBits $ vars
            ys' = reverse . take numBits . drop numBits $ vars
            rs' = reverse . drop (2*numBits) $ vars
            extraVars = drop (3*numBits) allVars
            createExtraVars
                | null extraVars = ""
                | otherwise = indent 4 $ "extraVars <- replicateM " ++ show (length extraVars) ++ " B.boolean"
            createResult = "rs <- replicateM " ++ show numBits ++ " B.boolean"
            clauseCode = unlines $ map printClause $ zip clauses [1..]
            printClause ((Or lits), i) = indent 4 $ "clause" ++ show i ++ " <- B.or [" ++ intercalate ", " (map mapLit lits) ++ "]"
            printAnd = "B.assert [" ++ intercalate ", " (map (\i -> "clause" ++ show i) [1..length clauses]) ++ "]"
            returnRs = "return rs"
            mapVar v
                | v `elem` xs' = str "xs" xs'
                | v `elem` ys' = str "ys" ys'
                | v `elem` rs' = str "rs" rs'
                | v `elem` extraVars = str "extraVars" extraVars
                where str listName list = listName ++ "!!" ++ show (fromJust $ elemIndex v list)
            mapLit lit = case lit of
                  Atom v -> mapVar v
                  Not (Atom v) -> "B.not (" ++ mapVar v ++ ")"
        in unlines . filter (/= "") $ [
            typeSignature,
            header,
            createExtraVars,
            indent 4 createResult,
            clauseCode,
            indent 4 printAnd,
            indent 4 returnRs
            ]


