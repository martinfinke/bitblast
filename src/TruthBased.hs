module TruthBased where

import Variable
import Formula
import NormalForm
import qualified TruthBasedCore as Core

import qualified Data.Set as Set
import qualified Data.Map as Map

minimizeTruthBased :: Int -> Formula -> IO Formula
minimizeTruthBased numExtraVars f =
    let varSet = variableSet f
        vars = Set.toAscList $ variableSet f
        numVars = length vars
        newVars = take numExtraVars $ newVariables varSet 
        convertedF = toCoreFormula vars f
    in fmap (fromCoreCNF $ vars ++ newVars) $ Core.optimize numVars convertedF numExtraVars

toCoreFormula :: [Variable] -> Formula -> (Core.Assignment -> Bool)
toCoreFormula vars f = \bools ->
    let assignment = assignmentFromList $ zip vars bools
    in assignment `isModelOf` f

fromCoreCNF :: [Variable] -> Core.CNF -> Formula
fromCoreCNF variables (Core.CNF clauses) = 
    let mapping = Map.fromList $ zip Core.variableNumbers variables
        convertedClauses = map (\(Core.Clause lits) -> map (convertLiteral mapping) lits) clauses
    in And . map Or $ convertedClauses

convertLiteral :: Map.Map Int Variable -> Core.Lit -> Formula
convertLiteral mapping (Core.Lit varNumber)
    | varNumber >= 0 = atom
    | otherwise = Not atom
    where atom = Atom $ mapping Map.! (abs varNumber)