{-|
'Formula' type representing a boolean function in propositional logic, and functions to evaluate and convert to a 'TruthTable.TruthTable'.
-}
module Formula (Formula(..),
                eval,
                variableSet,
                toTruthTable,
                possibleAssignments,
                allBoolCombinations,
                highestVariableIndex
                ) where

import TruthTable (Variable, getVariable, setVariable, Assignment, allFalse, OutputValue(..), TruthTable, emptyTable, setOutputs, boolToOutputValue)
import Data.List (intercalate)
import qualified Data.Set as Set

-- | A boolean 'Formula' in propositional logic.
data Formula = Atom     Variable -- ^ A positive literal
             | Not      Formula -- ^ Negation
             | And     [Formula] -- ^ Conjunction
             | Or      [Formula] -- ^ Disjunction
             | Implies  Formula Formula -- ^ Implication
             | Xor     [Formula] -- ^ Exclusive Or
             | Equiv   [Formula] -- ^ Equivalence
    deriving (Eq, Ord)

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

-- | Evaluates a 'Formula' under a given 'Assignment' to 'True' or 'False'.
eval :: Assignment -- ^ Assigns a value to each 'Variable'
     -> Formula -- ^ The 'Formula' to evaluate
     -> Bool -- ^ The value of the 'Formula' under the given 'Assignment'
eval assignment formula = case formula of
    Atom v -> getVariable v assignment
    Not f -> not $ eval assignment f
    And fs -> all (eval assignment) fs
    Or fs -> any (eval assignment) fs
    Implies premise conclusion -> if eval assignment premise then eval assignment conclusion else True
    Xor fs -> foldr (\f bool -> not $ eval assignment f == bool) False fs
    Equiv [] -> True
    Equiv (f:fs) -> all (== eval assignment f) (map (eval assignment) fs)

-- | The 'Set.Set' of all 'Variable's that appear in a given 'Formula'.
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

-- | Generates a 'TruthTable' from a given 'Formula'
toTruthTable :: Formula -> TruthTable
toTruthTable formula = setOutputs outputs (emptyTable tableSize)
    where assignments = possibleAssignments formula
          tableSize = highestVariableIndex formula + 1
          outputs = map (\assignment -> (assignment, boolToOutputValue $ eval assignment formula)) assignments

-- | All possible 'Assignment's for a given 'Formula', i.e. all combinations of true/false values for its 'variableSet'.
possibleAssignments :: Formula -> [Assignment]
possibleAssignments = allBoolCombinations . variableSet

-- | All possible 'Assignment's for a 'Set.Set' of 'Variable's.
allBoolCombinations :: Set.Set Variable -> [Assignment]
allBoolCombinations variables
    | Set.null variables = [allFalse]
    | otherwise = rest ++ map (setVariable variable True) rest
    where variable = Set.elemAt (Set.size variables - 1) variables
          rest = allBoolCombinations (Set.delete variable variables)

highestVariableIndex :: Formula -> Int
highestVariableIndex formula = case Set.toList variables of
    [] -> -1
    _ -> fromEnum $ Set.findMax variables
    where variables = variableSet formula
