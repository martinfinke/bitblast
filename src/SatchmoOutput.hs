module SatchmoOutput where

import Formula
import NormalForm
import Arithmetics
import MinimizeFormula(minimizeFormula, minimizeStructural, minimizeStructuralWithRange, minimizeTruthBased)
import CalculatedFormulas

import System.FilePath(joinPath)
import Data.List
import Data.Maybe
import Control.Monad(forM, forM_, replicateM)
import qualified Data.Set as Set
import Data.Char(toLower)

data OptSpec = Best -- ^ Use whatever encoding has the lowest number of literals
             | Method OptMethod Int -- ^ Use a certain OptMethod with a number of extra variables.
    deriving(Show,Eq)

outputNoExtra = outputToFile (Method NoExtraVars 0) minimizeFormula
outputStructural k =
    let minimizer = fmap fst . minimizeStructural k . toTree
    in outputToFile (Method Structural k) minimizer
outputTruthBased k = outputToFile (Method TruthBased k) (minimizeTruthBased k)
outputBest = outputToFile Best minimizeFormula

outputToFile :: OptSpec -> (Formula -> IO Formula) -> [Int] -> IO ()
outputToFile optSpec minimizer numBitsList =
    let outputDir = "../con-test/"
        moduleName = moduleNameForOptSpec optSpec
        fileName = moduleName ++ ".hs"
        outputFilename = joinPath [outputDir, fileName]
    in do
        str <- createSatchmo optSpec minimizer numBitsList
        writeFile outputFilename str

indent :: Int -> String -> String
indent amount str = replicate amount ' ' ++ str

moduleNameForOptSpec optSpec = "OptNat" ++ case optSpec of
    Best -> "Best"
    Method m k -> show m

createSatchmo :: OptSpec -> (Formula -> IO Formula) -> [Int] -> IO String
createSatchmo optSpec minimizer numBitsList =
    let moduleName = moduleNameForOptSpec optSpec
        header = unlines [
            "{-# LANGUAGE FlexibleInstances #-}",
            "{-# LANGUAGE MultiParamTypeClasses #-}",
            "{-# LANGUAGE FlexibleContexts #-}",
            "{-# LANGUAGE UndecidableInstances #-}",
            "module " ++ moduleName ++ " where",
            "import qualified Satchmo.Boolean as B",
            "import qualified Satchmo.Code as C",
            "import Semiring",
            "import Control.Monad",
            "import Control.Applicative",
            "import Data.Maybe",
            "import Data.List"
            ]
        operations = [
            nBitAddition Forbid,
            nBitMultiplication Forbid,
            greaterThan,
            greaterThanEq
            ]
        operationKeys = [Add, Mul, GreaterThan, GreaterThanEq]
        outputsResult  = [True, True, False, False]
        mkNewtype numBits =
            let typeName = moduleName ++ show numBits
            in "newtype " ++ typeName ++ " = " ++ typeName ++ " [B.Boolean]"
        mkDecodeInstance numBits =
            let typeName = moduleName ++ show numBits
            in "instance C.Decode m B.Boolean Bool => C.Decode m " ++ typeName ++ " Integer where\n" ++ unlines [
                indent 4 $ "decode (" ++ typeName ++ " bs) = foldr (\\x y -> fromIntegral (fromEnum (x::Bool)) + 2*y) 0 <$> forM bs C.decode"
            ]
        mkSemiringInstance numBits =
            let typeName = moduleName ++ show numBits
                xs = "(" ++ typeName ++ " xs) "
                ys = "(" ++ typeName ++ " ys)"
                args = xs ++ ys
            in "instance B.MonadSAT m => Semiring m " ++ typeName ++ " where\n" ++
                unlines (map (indent 4) [
                    "unknown = fmap " ++ typeName ++ " $ replicateM " ++ show numBits ++ " B.boolean",
                    "plus " ++ args ++ " = fmap " ++ typeName ++ " $ add" ++ show numBits ++ " xs ys",
                    "times " ++ args ++ " = fmap " ++ typeName ++ " $ mul" ++ show numBits ++ " xs ys",
                    "positive " ++ xs ++ "= B.or xs",
                    "greater " ++ args ++ " = greaterthan" ++ show numBits ++ " xs ys",
                    "greater_equal " ++ args ++ " = greaterthaneq" ++ show numBits ++ " xs ys"
                    ])
        mkDefs numBits = do
            let ops = map ($ numBits) operations
            cnfs <- forM (zip operationKeys ops) $ \(opKey,op) -> do
                let maybeCnf = case optSpec of
                        Best -> getBest opKey numBits
                        Method m k -> getIfAvailable m k opKey numBits
                case maybeCnf of
                    Just cnf -> return cnf
                    Nothing -> do
                        putStrLn $ "Formula for " ++ show optSpec ++ "/" ++ show opKey ++ "/" ++ show numBits ++ " not minimized yet. Minimizing..."
                        cnf <- minimizer op
                        putStrLn $ "Minimized formula:"
                        putStrLn $ prettyPrint cnf
                        return cnf

            let withKey = zip3 operationKeys cnfs outputsResult
            let toSatchmo (op,cnf,returnResult) = cnfToSatchmo returnResult op cnf numBits
            return . unlines $ map toSatchmo withKey

    in do
        body <- forM numBitsList $ \numBits -> do
            defs <- mkDefs numBits
            return . unlines $ [mkNewtype numBits, mkDecodeInstance numBits, mkSemiringInstance numBits, defs]
        return . unlines $ header : body

cnfToSatchmo :: Bool -> ArithmeticOp -> Formula -> Int -> String
cnfToSatchmo returnResult op f numBits
    | not (isCnf f) = error $ "cnfToSatchmo: Formula is not a CNF:\n" ++ show f
    | otherwise =
        let functionName = map toLower (show op) ++ show numBits
            typeSignature = functionName ++ " :: B.MonadSAT m => [B.Boolean] -> [B.Boolean] -> m " ++ if returnResult
                then "[B.Boolean]"
                else "B.Boolean"
            header = functionName ++ " xs ys = do"

            clauses = normalFormChildren f
            allVars = Set.toAscList . variableSet $ f
            vars = take (3*numBits) allVars
            xs' = take numBits $ vars
            ys' = take numBits . drop numBits $ vars
            rs'
                | returnResult = drop (2*numBits) $ vars
                | otherwise = []
            extraVars
                | returnResult = drop (3*numBits) allVars
                | otherwise = drop (2*numBits) allVars
            createExtraVars
                | null extraVars = ""
                | otherwise = indent 4 $ "extraVars <- replicateM " ++ show (length extraVars) ++ " B.boolean"
            createResult = if returnResult
                then "rs <- replicateM " ++ show numBits ++ " B.boolean"
                else ""
            clauseCode = unlines $ map printClause $ zip clauses [1..]
            printClause ((Or lits), i)
                | returnResult = indent 4 $ "B.assert [" ++ intercalate ", " (map mapLit lits) ++ "]"
                | otherwise = indent 4 $ "clause" ++ show i ++ " <- B.or [" ++ intercalate ", " (map mapLit lits) ++ "]"
            printAnd = "cnf <- B.and [" ++ intercalate ", " (map (\i -> "clause" ++ show i) [1..length clauses]) ++ "]"

            returnRs = "return " ++ if returnResult then "rs" else "cnf"
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
            indent 4 $ if returnResult then "" else printAnd,
            indent 4 returnRs
            ]


