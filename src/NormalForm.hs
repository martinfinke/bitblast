{-|
Functions to convert 'Formula'e to canonical CNF/DNF.
-}
module NormalForm (Canonical,
                   getFormula,
                   ensureCanonical,
                   toCanonicalCnf,
                   toCanonicalDnf,
                   FormType(..),
                   getType,
                   isCnf,
                   isDnf,
                   isCanonical,
                   isConjunctionOfLiterals,
                   isDisjunctionOfLiterals,
                   normalFormChildren,
                   termLiterals,
                   getStats,
                   FormulaStats(..)) where

import Formula
import Variable
import qualified Data.Set as Set
import Text.Printf(printf)

-- | This type wraps a 'Formula' that is (or has been converted to) a canonical CNF or DNF. The only way to create a 'Canonical' value is through the 'ensureCanonical' \/ 'toCanonicalCnf' \/ 'toCanonicalDnf' functions.
data Canonical = CNF Formula
               | DNF Formula

instance Show Canonical where
    show = show . getFormula

-- | Extract a 'Formula' that has been wrapped
getFormula :: Canonical -> Formula
getFormula (CNF formula) = formula
getFormula (DNF formula) = formula

-- | Checks whether a 'Formula' is already in canonical form, and if not, converts it to a 'Canonical'. If it is already a DNF, it will become a 'Canonical' DNF. Otherwise, it will become a 'Canonical' CNF.
ensureCanonical :: Formula -> Canonical
ensureCanonical formula
    | isCanonical formula = if isCnf formula then CNF formula else DNF formula
    | isDnf formula = toCanonicalDnf formula
    | otherwise = toCanonicalCnf formula

toCanonicalCnf, toCanonicalDnf :: Formula -> Canonical
-- | Converts a 'Formula' to 'Canonical' CNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'F'.
toCanonicalCnf = toNormalForm CNFType
-- | Converts a 'Formula' to 'Canonical' DNF by creating a 'TruthTable.TruthTable' and reading all rows where the 'OutputValue' is 'T'.
toCanonicalDnf = toNormalForm DNFType

toNormalForm :: FormType -> Formula -> Canonical
toNormalForm formType formula = operator terms
    where truthTable = toTruthTable formula
          assignments = possibleAssignments formula
          terms = map (assignmentToTerm formType $ variableSet formula) onlyRelevantOutput
          relevantOutput = if formType == CNFType then Just False else Just True
          operator = if formType == CNFType then CNF . And else DNF . Or
          onlyRelevantOutput = filter (\assignment -> getRow assignment truthTable == relevantOutput) assignments

-- | Extract the type of a 'Canonical' formula.
getType :: Canonical -> FormType
getType (CNF _) = CNFType
getType (DNF _) = DNFType

-- | The type of a ('Canonical') formula: Either CNF or DNF.
data FormType = CNFType | DNFType
    deriving(Eq, Show)

assignmentToTerm :: FormType -> Set.Set Variable -> Assignment -> Formula
assignmentToTerm formType variables assignment = operator $ Set.foldr addLiteral [] variables
    where operator = if formType == CNFType then Or else And
          addLiteral variable literals =
            let ifTrue = if formType == CNFType then Not (Atom variable) else Atom variable
                ifFalse = if formType == CNFType then Atom variable else Not (Atom variable)
            in case getVar variable assignment of
                Just True -> ifTrue : literals
                Just False -> ifFalse : literals
                Nothing -> error $ show assignment ++ " is incomplete for variableSet: " ++ show variables

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

-- | Extract the conjunctions/disjunctions of a DNF/CNF, or the literals of a conjunction/disjunction.
normalFormChildren :: Formula -> [Formula]
normalFormChildren (And children) = children
normalFormChildren (Or children) = children
normalFormChildren invalidFormula = error $ "Not a normal form: " ++ show invalidFormula

-- | The 'Set' of literals of a term
termLiterals :: Formula -> Set.Set Formula
termLiterals = Set.fromList . normalFormChildren

data FormulaStats = FormulaStats {numClauses::Int, numLiterals::Int}
    deriving(Eq)

instance Show FormulaStats where
    show (FormulaStats {numClauses=clauses, numLiterals=literals}) = printf "(%d,%d) (clauses/literals)" clauses literals

getStats :: Formula -> FormulaStats
getStats f
    | isCnf f || isDnf f = FormulaStats {numClauses=length clauses,numLiterals=length literals}
    | otherwise = error "getStats needs a CNF or DNF."
    where clauses = normalFormChildren f
          literals = concatMap normalFormChildren clauses