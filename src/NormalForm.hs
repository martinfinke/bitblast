{-|
Functions to convert 'Formula'e to canonical CNF/DNF.
-}
module NormalForm (toCanonicalCnf,
                   toCanonicalDnf,
                   isCnf,
                   isDnf,
                   isCanonical,
                   isMinterm,
                   isMaxterm,
                   isLiteral,
                   isPositiveLiteral,
                   assignmentToMinterm,
                   assignmentToMaxterm,
                   ensureCanonical) where

import Formula
import TruthTable (Variable, Assignment, getVariable, OutputValue(..), getOutput)
import qualified Data.Set as Set

toCanonicalCnf, toCanonicalDnf :: Formula -> Formula
-- | Converts a 'Formula' to canonical CNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'F'.
toCanonicalCnf = toNormalForm CNFType
-- | Converts a 'Formula' to canonical DNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'T'.
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
-- | Converts an 'Assignment' (i.e. a row in a 'TruthTable.TruthTable') to a minterm for a DNF. The 'Formula.variableSet' has to be passed as well.
assignmentToMinterm = assignmentToTerm DNFType
-- | Converts an 'Assignment' (i.e. a row in a 'TruthTable.TruthTable') to a maxterm (clause) for a CNF. The 'Formula.variableSet' has to be passed as well.
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
-- | Checks whether a 'Formula' is a conjunction of clauses (disjuncts). Doesn't check if it is canonical.
isCnf (And clauses) = all isMaxterm clauses
isCnf _ = False

-- | Checks whether a 'Formula' is a disjunction of terms (conjuncts). Doesn't check if it is canonical.
isDnf (Or terms) = all isMinterm terms
isDnf _ = False

-- | Checks whether a 'Formula' is canonical. This is true iff (1) it is CNF or DNF, and (2) each 'Variable' in the 'Formula.variableSet' appears exactly once in every clause/term.
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

isMinterm, isMaxterm :: Formula -> Bool
-- | Checks whether a given 'Formula' is a maxterm (clause). A maxterm is a disjunction of literals.
isMaxterm t = case t of
    (Or literals) -> all isLiteral literals
    _ -> False

-- | Checks whether a given 'Formula' is a minterm. A minterm is a conjunction of literals.
isMinterm t = case t of
    (And literals) -> all isLiteral literals
    _ -> False

-- | Checks whether a given 'Formula' is a literal. A literal is an 'Formula.Atom' or a negated 'Formula.Atom'.
isLiteral :: Formula -> Bool
isLiteral (Atom _) = True
isLiteral (Not (Atom _)) = True
isLiteral _ = False

-- | Checks whether a literal is positive (i.e. not negated).
isPositiveLiteral :: Formula -> Bool
isPositiveLiteral (Atom _) = True
isPositiveLiteral _ = False

-- | Checks whether a 'Formula' is canonical, and if not, converts it to canonical form. If it is already a DNF, it will become a canonical DNF. Otherwise, it will become a canonical CNF.
ensureCanonical :: Formula -> Formula
ensureCanonical formula
    | isCanonical formula = formula
    | isDnf formula = toCanonicalDnf formula
    | otherwise = toCanonicalCnf formula
