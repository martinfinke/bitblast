module TruthBased(
                  minimizeTruthBased,
                  minimizeTruthBasedQmc,
                  toTable,
                  toTableQmc,
                  toCoreFormula,
                  fromCoreCNF,
                  toBitVector,
                  fromQmTerm,
                  convertLiteral) where

import Variable
import Formula
import NormalForm
import QmcTypes
import ModifiedQmc(modifiedQmc)
import qualified TruthBasedCore as Core
import qualified Data.Bits as B
import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map as Map

minimizeTruthBased, minimizeTruthBasedQmc :: Int -> Formula -> IO Formula
minimizeTruthBased = minimizeTruthBasedWith Core.defaultOptions

minimizeTruthBasedQmc = minimizeTruthBasedWith opts
    where opts = Core.defaultOptions{Core.clauseProvider=modifiedQmcClauseProvider}

minimizeTruthBasedWith options numExtraVars f =
    fmap (fromCoreCNF $ variables f ++ newVars numExtraVars f) $ common numExtraVars f (Core.optimizeWith options)

toTable :: Int -> Formula -> Core.Table
toTable = toTableWith Core.defaultOptions
toTableQmc = toTableWith opts
    where opts = Core.defaultOptions{Core.clauseProvider=modifiedQmcClauseProvider}
toTableWith opts numExtraVars f = common numExtraVars f (Core.tableWith opts)

common numExtraVars f operation =
    let numVars = length (variables f)
        convertedF = toCoreFormula (variables f) f
    in operation numVars convertedF numExtraVars

variables :: Formula -> [Variable]
variables = Set.toAscList . variableSet

newVars :: Int -> Formula -> [Variable]
newVars numExtraVars = take numExtraVars . newVariables . variableSet

toCoreFormula :: [Variable] -> Formula -> (Core.Assignment -> Bool)
toCoreFormula vars f = \bools ->
    let assignment = assignmentFromList $ zip vars bools
    in assignment `isModelOf` f

fromCoreCNF :: [Variable] -> Core.CNF -> Formula
fromCoreCNF vars (Core.CNF clauses) = 
    let mapping = Map.fromList $ zip Core.variableNumbers vars
        convertedClauses = map (\(Core.Clause lits) -> map (convertLiteral mapping) lits) clauses
    in And . map Or $ convertedClauses

convertLiteral :: Map.Map Int Variable -> Core.Lit -> Formula
convertLiteral mapping (Core.Lit varNumber)
    | varNumber >= 0 = atom
    | otherwise = Not atom
    where atom = Atom $ mapping Map.! (abs varNumber)

toBitVector :: Core.Assignment -> BitVector
toBitVector a = foldr setBitAtIndex 0 $ zip [0..] a
    where setBitAtIndex (i,b) bv = if b then B.setBit bv i else bv

fromQmTerm :: Int -> QmTerm -> Core.Clause
fromQmTerm totalNumVars qmTerm =
    let mask = getMask qmTerm
        term = getTerm qmTerm
        check i lits = if not (B.testBit mask i) 
                        then Core.lit (i+1) (not $ B.testBit term i) : lits
                        else lits
    in Core.Clause $ foldr check [] [0..totalNumVars-1]

modifiedQmcClauseProvider :: Int -> [Core.Assignment] -> [Core.Assignment] -> [Core.Clause]
modifiedQmcClauseProvider totalNumVars ones zeros =
    let ones' = map toBitVector ones
        zeros' = map toBitVector zeros
        qmTerms = modifiedQmc zeros' ones'
    in map (fromQmTerm totalNumVars) qmTerms