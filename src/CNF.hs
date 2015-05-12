module CNF (CNF(..),
            cnfFromList,
            cnfToList,
            Clause(..),
            clauseFromList,
            clauseToList,
            Literal(..),
            varFromLiteral,
            VarName) where

import Data.List (sort)

andSign, orSign, negationSign :: String
andSign = "&"
orSign = "|"
negationSign = "-"

parens :: String -> String
parens s = '(' : s ++ ")"

data CNF = And Clause CNF
         | EmptyTrue

instance Show CNF where
    show EmptyTrue = ""
    show (And cls restCnf) = combine lhsString rhsString
        where clsString = show cls
              restCnfString = show restCnf
              lhsString = if null clsString then "" else parens clsString
              rhsString = if null restCnfString then "" else restCnfString

combine :: String -> String -> String
combine "" "" = ""
combine "" rhs = rhs
combine lhs "" = lhs
combine lhs rhs = lhs ++ " " ++ andSign ++ " " ++ rhs

cnfFromList :: [[Literal]] -> CNF
cnfFromList = foldr addIfNonEmpty EmptyTrue

addIfNonEmpty :: [Literal] -> CNF -> CNF
addIfNonEmpty [] restCnf = restCnf
addIfNonEmpty literals restCnf = (And . clauseFromList) literals restCnf

cnfToList :: CNF -> [[Literal]]
cnfToList EmptyTrue = []
cnfToList (And cls restCnf) = if null currentClause
                                then rest
                                else currentClause : rest
    where currentClause = clauseToList cls
          rest = cnfToList restCnf

data Clause = Or Literal Clause
            | EmptyFalse

instance Show Clause where
    show EmptyFalse = ""
    show (Or lit EmptyFalse) = show lit
    show (Or lit restClause) = show lit ++ " " ++ orSign ++ " " ++ show restClause

instance Eq Clause where
    lhs == rhs = (sort . clauseToList) lhs == (sort . clauseToList) rhs

clauseFromList :: [Literal] -> Clause
clauseFromList = foldr Or EmptyFalse

clauseToList :: Clause -> [Literal]
clauseToList EmptyFalse = []
clauseToList (Or lit restClause) = lit : clauseToList restClause

data Literal = Pos VarName
             | Neg VarName
    deriving (Eq)

instance Ord Literal where
    lit1 `compare` lit2 = varFromLiteral lit1 `compare` varFromLiteral lit2

instance Show Literal where
    show (Pos varName) = varName
    show (Neg varName) = negationSign ++ varName

varFromLiteral :: Literal -> VarName
varFromLiteral (Pos varName) = varName
varFromLiteral (Neg varName) = varName

type VarName = String