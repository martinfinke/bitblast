{-|
'Formula' type representing a boolean function in propositional logic, and functions to evaluate and convert to a 'TruthTable.TruthTable'.
-}
module Formula (Formula(..),
                prettyPrint,
                isModelOf,
                variableSet,
                numVars,
                toTruthTable,
                possibleAssignments,
                allBoolCombinations,
                isLiteral,
                isPositiveLiteral,
                toTree,
                equiv,
                equisatGTE
                ) where

import Variable hiding(prettyPrint)
import Assignment
import TruthTable
import qualified Variable as V
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
    deriving (Eq, Ord, Show)

prettyPrint :: Formula -> String
prettyPrint f = case f of
    Atom v -> V.prettyPrint v
    Not f' -> '-' : prettyPrint f'
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
          parensJoin delimiter fs = parens $ intercalate delimiter $ map prettyPrint fs

-- | Evaluates a 'Formula' under a given 'Assignment' to 'True' or 'False'.
isModelOf :: Assignment -- ^ Assigns a value to each 'Variable'
     -> Formula -- ^ The 'Formula' to evaluate
     -> Bool -- ^ The value of the 'Formula' under the given 'Assignment'
assignment `isModelOf` formula = case formula of
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

numVars :: Formula -> Int
numVars = Set.size . variableSet

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

-- | Converts a Formula to tree form. This means that all And, Or and Xor lists will be converted to nested binary operators with two operands each.
toTree :: Formula -> Formula
toTree formula = case formula of
    Atom v -> formula
    Not f -> Not (toTree f)
    And fs -> treeify And fs
    Or fs -> treeify Or fs
    Implies premise conclusion -> Implies (toTree premise) (toTree conclusion)
    Xor fs -> treeify Xor fs
    -- Equiv must not be converted because it's not associative:
    Equiv fs -> Equiv fs
    where treeify op fs = case fs of
            [] -> op []
            (f:[]) -> f
            (f:f':[]) -> op [toTree f, toTree f']
            (f:fs) -> op [toTree f, treeify op fs]


equiv :: Formula -> Formula -> Bool
equiv f1 f2 = toTruthTable f1 == toTruthTable f2

-- | Naive equisatisfiability test. Takes a long time (of course) for formulas with many variables.
equisatGTE :: Formula -> Formula -> Bool
equisatGTE ex base
    | not $ baseVars `Set.isSubsetOf` exVars = False
    | otherwise = trimmed == toTruthTable base
    where exVars = variableSet ex
          baseVars = variableSet base
          trimmed = trim baseVars $ toTruthTable ex
