module CNF (CNF(..),
            cnfFromList,
            cnfToList,
            Clause(..),
            clauseFromList,
            clauseToList,
            Literal(..),
            VarName) where


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
cnfFromList = foldr (And . clauseFromList) EmptyTrue

cnfToList :: CNF -> [[Literal]]
cnfToList EmptyTrue = []
cnfToList (And cls restCnf) = clauseToList cls : cnfToList restCnf

data Clause = Or Literal Clause
            | EmptyFalse

instance Show Clause where
    show EmptyFalse = ""
    show (Or lit EmptyFalse) = show lit
    show (Or lit restClause) = show lit ++ " " ++ orSign ++ " " ++ show restClause

clauseFromList :: [Literal] -> Clause
clauseFromList = foldr Or EmptyFalse

clauseToList :: Clause -> [Literal]
clauseToList EmptyFalse = []
clauseToList (Or lit restClause) = lit : clauseToList restClause

data Literal = Pos VarName
             | Neg VarName

instance Show Literal where
    show (Pos varName) = varName
    show (Neg varName) = negationSign ++ varName

type VarName = String