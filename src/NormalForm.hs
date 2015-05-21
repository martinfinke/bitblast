module NormalForm (toCanonicalCnf,
                   toCanonicalDnf,
                   isCnf,
                   isDnf,
                   isMinterm,
                   isMaxterm,
                   isLiteral,
                   isPositiveLiteral,
                   FormType(..),
                   assignmentToMinterm,
                   assignmentToMaxterm,
                   isCanonical,
                   ensureCanonical) where

import Formula
import TruthTable (Variable, Assignment, getVariable, OutputValue(..), getOutput)
import qualified Data.Set as Set

toCanonicalCnf, toCanonicalDnf :: Formula -> Formula
toCanonicalCnf = toNormalForm CNFType
toCanonicalDnf = toNormalForm DNFType

toNormalForm :: FormType -> Formula -> Formula
toNormalForm formType formula = operator maxterms
    where truthTable = toTruthTable formula
          assignments = possibleAssignments formula
          maxterms = map (assignmentToTerm formType $ variableSet formula) onlyRelevantOutput
          relevantOutput = if formType == CNFType then F else T
          operator = if formType == CNFType then And else Or
          onlyRelevantOutput = filter (\assignment -> getOutput assignment truthTable == relevantOutput) assignments

data FormType = CNFType | DNFType
    deriving(Eq)

assignmentToMinterm, assignmentToMaxterm :: Set.Set Variable -> Assignment -> Formula
assignmentToMinterm = assignmentToTerm DNFType
assignmentToMaxterm = assignmentToTerm CNFType

assignmentToTerm :: FormType -> Set.Set Variable -> Assignment -> Formula
assignmentToTerm formType variables assignment = operator $ Set.foldr addLiteral [] variables
    where operator = if formType == CNFType then Or else And
          addLiteral variable literals =
            let ifTrue = if formType == CNFType then Not (Atom variable) else Atom variable
                ifFalse = if formType == CNFType then Atom variable else Not (Atom variable)
            in if getVariable variable assignment
                then ifTrue : literals
                else ifFalse : literals

isCnf, isDnf :: Formula -> Bool
isCnf (And clauses) = all isMaxterm clauses
isCnf _ = False

isDnf (Or terms) = all isMinterm terms
isDnf _ = False

isMinterm, isMaxterm :: Formula -> Bool
isMaxterm t = case t of
    (Or literals) -> all isLiteral literals
    _ -> False

isMinterm t = case t of
    (And literals) -> all isLiteral literals
    _ -> False

isLiteral :: Formula -> Bool
isLiteral (Atom _) = True
isLiteral (Not (Atom _)) = True
isLiteral _ = False

isPositiveLiteral :: Formula -> Bool
isPositiveLiteral (Atom _) = True
isPositiveLiteral _ = False

isCanonical :: Formula -> Bool
isCanonical formula
    | isCnf formula = let (And maxterms) = formula in all canonical maxterms
    | isDnf formula = let (Or minterms) = formula in all canonical minterms
    | otherwise = False
    where canonical term = variableSet term == variables && termLength term == Set.size variables
          variables = variableSet formula
          termLength term = case term of
            (Or literals) -> length literals
            (And literals) -> length literals

ensureCanonical :: Formula -> Formula
ensureCanonical formula
    | isCanonical formula = formula
    | isDnf formula = toCanonicalDnf formula
    | otherwise = toCanonicalCnf formula
