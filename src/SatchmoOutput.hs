module SatchmoOutput where

import Formula
import NormalForm
import Arithmetics
import MinimizeFormula(minimizeFormula, minimizeWithExtraVarRange)
import TruthBased(minimizeTruthBased)
import CalculatedFormulas

import Data.List
import Data.Maybe
import Control.Monad(forM, forM_, replicateM)
import qualified Data.Set as Set

outputNoExtra = createSatchmo "OptNatNoExtra" minimizeFormula Nothing
outputStructural =
    let minimizer = fmap fst . minimizeWithExtraVarRange (0,1) . toTree
    in createSatchmo "OptNatStruct" minimizer (Just 1)
outputTruthBased = createSatchmo "OptNatTruth" (minimizeTruthBased 1) (Just 1)


indent :: Int -> String -> String
indent amount str = replicate amount ' ' ++ str

createSatchmo :: String -> (Formula -> IO Formula) -> Maybe Int -> [Int] -> IO String
createSatchmo name minimizer maybeNumExtraVars numBitsList =
    let fileName = name ++ ".hs"
        header = unlines [
            "{-# LANGUAGE FlexibleInstances #-}",
            "{-# LANGUAGE MultiParamTypeClasses #-}",
            "{-# LANGUAGE FlexibleContexts #-}",
            "{-# LANGUAGE UndecidableInstances #-}",
            "module " ++ name ++ " where",
            "import qualified Satchmo.Boolean as B",
            "import qualified Satchmo.Code as C",
            "import Semiring",
            "import Control.Monad",
            "import Control.Applicative",
            "import Data.Maybe",
            "import Data.List"
            ]
        operations = [
            fst . nBitAddition Forbid,
            fst . nBitMultiplication Forbid,
            getFormula . fst . greaterThan,
            getFormula . fst . greaterThanEq
            ]
        operationNames = ["add", "mul", "gt", "ge"]
        outputsResult  = [True, True, False, False]
        mkNewtype numBits =
            let typeName = name ++ show numBits
            in "newtype " ++ typeName ++ " = " ++ typeName ++ " [B.Boolean]"
        mkDecodeInstance numBits =
            let typeName = name ++ show numBits
            in "instance C.Decode m B.Boolean Bool => C.Decode m " ++ typeName ++ " Integer where\n" ++ unlines [
                indent 4 $ "decode (" ++ typeName ++ " bs) = foldr (\\x y -> fromIntegral (fromEnum (x::Bool)) + 2*y) 0 <$> forM bs C.decode"
            ]
        mkSemiringInstance numBits =
            let typeName = name ++ show numBits
                xs = "(" ++ typeName ++ " xs) "
                ys = "(" ++ typeName ++ " ys)"
                args = xs ++ ys
            in "instance B.MonadSAT m => Semiring m " ++ typeName ++ " where\n" ++
                unlines (map (indent 4) [
                    "unknown = fmap " ++ typeName ++ " $ replicateM " ++ show numBits ++ " B.boolean",
                    "plus " ++ args ++ " = fmap " ++ typeName ++ " $ add" ++ show numBits ++ " xs ys",
                    "times " ++ args ++ " = fmap " ++ typeName ++ " $ mul" ++ show numBits ++ " xs ys",
                    "positive " ++ xs ++ "= B.or xs",
                    "greater " ++ args ++ " = gt" ++ show numBits ++ " xs ys",
                    "greater_equal " ++ args ++ " = ge" ++ show numBits ++ " xs ys"
                    ])
        mkDefs numBits = do
            let ops = map ($ numBits) operations
            cnfs <- forM (zip operationNames ops) $ \(opname,op) -> do
                let methodName = case name of
                        "OptNatStruct" -> name ++ show (fromJust maybeNumExtraVars) ++ "Ex"
                        "OptNatTruth" -> name ++ show (fromJust maybeNumExtraVars) ++ "Ex"
                        _ -> name
                case getIfAvailable methodName opname numBits of
                    Just cnf -> return cnf
                    Nothing -> do
                        putStrLn $ "Formula for " ++ methodName ++ "/" ++ opname ++ "/" ++ show numBits ++ " not minimized yet. Minimizing..."
                        cnf <- minimizer op
                        putStrLn $ "Minimized formula:"
                        putStrLn $ prettyPrint cnf
                        return cnf

            let withName = zip3 operationNames cnfs outputsResult
            let toSatchmo (opName,cnf,returnResult) = cnfToSatchmo returnResult opName cnf numBits
            return . unlines $ map toSatchmo withName

    in do
        body <- forM numBitsList $ \numBits -> do
            defs <- mkDefs numBits
            return . unlines $ [mkNewtype numBits, mkDecodeInstance numBits, mkSemiringInstance numBits, defs]
        return . unlines $ header : body

cnfToSatchmo :: Bool -> String -> Formula -> Int -> String
cnfToSatchmo returnResult name f numBits
    | not (isCnf f) = error $ "cnfToSatchmo: Formula is not a CNF:\n" ++ show f
    | otherwise =
        let functionName = name ++ show numBits
            typeSignature = functionName ++ " :: B.MonadSAT m => [B.Boolean] -> [B.Boolean] -> m " ++ if returnResult
                then "[B.Boolean]"
                else "B.Boolean"
            header = functionName ++ " xs ys = do"

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
            createResult = if returnResult
                then "rs <- replicateM " ++ show numBits ++ " B.boolean"
                else ""
            clauseCode = unlines $ map printClause $ zip clauses [1..]
            printClause ((Or lits), i) = indent 4 $ "clause" ++ show i ++ " <- B.or [" ++ intercalate ", " (map mapLit lits) ++ "]"
            printAnd = "cnf <- B.and [" ++ intercalate ", " (map (\i -> "clause" ++ show i) [1..length clauses]) ++ "]"
            returnRs = if returnResult
                then "B.assert [cnf]; return rs"
                else "return cnf"
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


