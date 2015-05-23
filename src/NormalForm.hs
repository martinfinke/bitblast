{-|
Functions to convert 'Formula'e to canonical CNF/DNF.
-}
module NormalForm (toCanonicalCnf,
                   toCanonicalDnf,
                   FormType(..),
                   checkCanonical,
                   isCnf,
                   isDnf,
                   isCanonical,
                   isConjunctionOfLiterals,
                   isDisjunctionOfLiterals,
                   isLiteral,
                   isPositiveLiteral,
                   ensureCanonical,
                   normalFormChildren,
                   termLiterals) where

import Formula
import TruthTable (Variable, Assignment, getVariable, getOutput)
import qualified Data.Set as Set

toCanonicalCnf, toCanonicalDnf :: Formula -> Formula
-- | Converts a 'Formula' to canonical CNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'F'.
toCanonicalCnf = toNormalForm CNFType
-- | Converts a 'Formula' to canonical DNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'T'.
toCanonicalDnf = toNormalForm DNFType

toNormalForm :: FormType -> Formula -> Formula
toNormalForm formType formula = operator terms
    where truthTable = toTruthTable formula
          assignments = possibleAssignments formula
          terms = map (assignmentToTerm formType $ variableSet formula) onlyRelevantOutput
          relevantOutput = if formType == CNFType then Just False else Just True
          operator = if formType == CNFType then And else Or
          onlyRelevantOutput = filter (\assignment -> getOutput assignment truthTable == relevantOutput) assignments

data FormType = CNFType | DNFType
    deriving(Eq, Show)

checkCanonical :: Formula -> Maybe FormType
checkCanonical formula
    | isCanonical formula = Just $ if isCnf formula then CNFType else DNFType
    | otherwise = Nothing

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
-- | Checks whether a 'Formula' is a conjunction of clauses (disjunctions of literals). Doesn't check if it is canonical.
isCnf (And clauses) = all isDisjunctionOfLiterals clauses
isCnf _ = False

-- | Checks whether a 'Formula' is a disjunction of conjunctions of literals. Doesn't check if it is canonical.
isDnf (Or terms) = all isConjunctionOfLiterals terms
isDnf _ = False

-- | Checks whether a 'Formula' is canonical. This is true iff (1) it is CNF or DNF, and (2) each 'Variable' in the 'Formula.variableSet' appears exactly once in every clause/term.
isCanonical :: Formula -> Bool
isCanonical formula
    | isCnf formula || isDnf formula = all canonical (normalFormChildren formula)
    | otherwise = False
    where canonical term = variableSet term == variables && termLength term == Set.size variables
          variables = variableSet formula
          termLength term = case term of
            (Or literals) -> length literals
            (And literals) -> length literals

isConjunctionOfLiterals, isDisjunctionOfLiterals :: Formula -> Bool
-- | Checks whether a given 'Formula' is a disjunction of literals (clause).
isDisjunctionOfLiterals t = case t of
    (Or literals) -> all isLiteral literals
    _ -> False

-- | Checks whether a given 'Formula' is a conjunction of literals.
isConjunctionOfLiterals t = case t of
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

-- | Extract the conjunctions/disjunctions of a DNF/CNF, or the literals of a conjunction/disjunction.
normalFormChildren :: Formula -> [Formula]
normalFormChildren (And children) = children
normalFormChildren (Or children) = children
normalFormChildren invalidFormula = error $ "Not a normal form: " ++ show invalidFormula

-- | The 'Set' of literals of a term
termLiterals :: Formula -> Set.Set Formula
termLiterals = Set.fromList . normalFormChildren