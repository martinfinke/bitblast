module Formula where

import TruthTable (Variable, var, Assignment, getVariable)
import qualified Prelude as P
import Prelude hiding (not,and,or)
import Data.List (intercalate)

data Formula = Atom     Variable
             | Not      Formula
             | And     [Formula]
             | Or      [Formula]
             | Implies  Formula Formula
             | Xor     [Formula]
             | Equiv   [Formula]

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
    Atom v -> getVariable assignment v
    Not f -> P.not $ eval assignment f
    And fs -> P.all (eval assignment) fs
    Or fs -> P.any (eval assignment) fs
    Implies premise conclusion -> if eval assignment premise then eval assignment conclusion else True
    Xor fs -> foldr (\f bool -> P.not $ eval assignment f == bool) False fs
    Equiv [] -> True
    Equiv (f:fs) -> P.all (== eval assignment f) (map (eval assignment) fs)

