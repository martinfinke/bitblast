module Formula (Formula(..),
                eval,
                variableSet,
                numVariablesInFormula,
                allBoolCombinations,
                possibleAssignments,
                toTruthTable,
                toCanonicalCnf,
                toCanonicalDnf,
                isCnf,
                isDnf,
                isMinterm,
                isMaxterm,
                isLiteral,
                isPositiveLiteral,
                FormType(..),
                assignmentToMinterm,
                assignmentToMaxterm
                ) where

import TruthTable (Variable, var, Assignment, allFalse, getVariable, setVariable, TruthTable, emptyTable, setOutputs, boolToOutputValue, getOutput, OutputValue(..))
import qualified Prelude as P
import Prelude hiding (not,and,or)
import Data.List (intercalate)
import qualified Data.Set as Set

data Formula = Atom     Variable
             | Not      Formula
             | And     [Formula]
             | Or      [Formula]
             | Implies  Formula Formula
             | Xor     [Formula]
             | Equiv   [Formula]
    deriving (Eq)

instance Show Formula where
    show f = case f of
        Atom v -> show v
        Not f' -> '-' : show f'
        And [] -> "true"
        And fs -> parensJoin " && " fs
        Or [] -> "false"
        Or fs -> parensJoin " || " fs
        Implies premise conclusion -> parensJoin " -> " [premise, conclusion]
        Xor [] -> "false"
        Xor fs -> parensJoin " XOR " fs
        Equiv [] -> "true"
        Equiv (_:[]) -> "true"
        Equiv fs -> parensJoin " <=> " fs
        where surround (open, close) string = open ++ string ++ close
              parens = surround ("(", ")")
              parensJoin delimiter fs = parens $ intercalate delimiter $ map show fs

eval :: Assignment -> Formula -> Bool
eval assignment formula = case formula of
    Atom v -> getVariable v assignment
    Not f -> P.not $ eval assignment f
    And fs -> P.all (eval assignment) fs
    Or fs -> P.any (eval assignment) fs
    Implies premise conclusion -> if eval assignment premise then eval assignment conclusion else True
    Xor fs -> foldr (\f bool -> P.not $ eval assignment f == bool) False fs
    Equiv [] -> True
    Equiv (f:fs) -> P.all (== eval assignment f) (map (eval assignment) fs)

variableSet :: Formula -> Set.Set Variable
variableSet formula = case formula of
    Atom v -> Set.fromList [v]
    Not f -> variableSet f
    And fs -> foldList fs
    Or fs -> foldList fs
    Implies premise conclusion -> foldList [premise, conclusion]
    Xor fs -> foldList fs
    Equiv fs -> foldList fs
    where foldList = foldr (Set.union . variableSet) Set.empty

numVariablesInFormula :: Formula -> Int
numVariablesInFormula = Set.size . variableSet

highestVariableIndex :: Formula -> Int
highestVariableIndex formula = case Set.toList variables of
    [] -> -1
    _ -> fromEnum $ Set.findMax variables
    where variables = variableSet formula

possibleAssignments :: Formula -> [Assignment]
possibleAssignments = allBoolCombinations . variableSet

allBoolCombinations :: Set.Set Variable -> [Assignment]
allBoolCombinations variables
    | Set.null variables = [allFalse]
    | otherwise = rest ++ map (setVariable variable True) rest
    where variable = Set.elemAt (Set.size variables - 1) variables
          rest = allBoolCombinations (Set.delete variable variables)

toTruthTable :: Formula -> TruthTable
toTruthTable formula = setOutputs outputs (emptyTable tableSize)
    where assignments = possibleAssignments formula
          tableSize = highestVariableIndex formula + 1
          outputs = map (\assignment -> (assignment, boolToOutputValue $ eval assignment formula)) assignments

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
isCnf (And clauses) = P.all isMaxterm clauses
isCnf _ = False

isDnf (Or terms) = P.all isMinterm terms
isDnf _ = False

isMinterm, isMaxterm :: Formula -> Bool
isMaxterm t = case t of
    (Or literals) -> P.all isLiteral literals
    _ -> False

isMinterm t = case t of
    (And literals) -> P.all isLiteral literals
    _ -> False

isLiteral :: Formula -> Bool
isLiteral (Atom _) = True
isLiteral (Not (Atom _)) = True
isLiteral _ = False

isPositiveLiteral :: Formula -> Bool
isPositiveLiteral (Atom _) = True
isPositiveLiteral _ = False