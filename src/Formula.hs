{-|
'Formula' type representing a boolean function in propositional logic, and functions to evaluate and convert to a 'TruthTable.TruthTable'.
-}
module Formula (Formula(..),
                isModelOf,
                variableSet,
                toTruthTable,
                possibleAssignments,
                allBoolCombinations,
                isLiteral,
                isPositiveLiteral
                ) where

import Variable hiding(eval)
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
isModelOf :: Assignment -- ^ Assigns a value to each 'Variable'
     -> Formula -- ^ The 'Formula' to evaluate
     -> Bool -- ^ The value of the 'Formula' under the given 'Assignment'
isModelOf assignment formula = case formula of
    Atom v -> case getVar v assignment of
        Nothing -> error $ "Variable not assigned: " ++ show v
        Just b -> b
    Not f -> not $ assignment `isModelOf` f
    And fs -> all (assignment `isModelOf`) fs
    Or fs -> any (assignment `isModelOf`) fs
    Implies premise conclusion -> if assignment `isModelOf` premise then assignment `isModelOf` conclusion else True
    Xor fs -> (odd . length . filter (assignment `isModelOf`)) fs
    Equiv [] -> True
    Equiv (f:fs) -> all (== assignment `isModelOf` f) (map (assignment `isModelOf`) fs)

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
toTruthTable formula = tableFromList outputs
    where assignments = possibleAssignments formula
          outputs = map (\assignment -> (assignment, assignment `isModelOf` formula)) assignments

-- | All possible 'Assignment's for a given 'Formula', i.e. all combinations of true/false values for its 'variableSet'.
possibleAssignments :: Formula -> [Assignment]
possibleAssignments = allBoolCombinations . variableSet

-- | Checks whether a given 'Formula' is a literal. A literal is an 'Formula.Atom' or a negated 'Formula.Atom'.
isLiteral :: Formula -> Bool
isLiteral (Atom _) = True
isLiteral (Not (Atom _)) = True
isLiteral _ = False

-- | Checks whether a literal is positive (i.e. not negated).
isPositiveLiteral :: Formula -> Bool
isPositiveLiteral (Atom _) = True
isPositiveLiteral _ = False
