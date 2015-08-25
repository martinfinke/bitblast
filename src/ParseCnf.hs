{-# language FlexibleContexts #-}
module ParseCnf where

import Formula
import Variable

import Text.Parsec
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Combinator as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)

negation = do
    minusSign <- char '-'
    a <- atom
    return $ Not a

atom = do
    num <- read <$> many1 digit
    return $ Atom (vars!!fromIntegral num)
    where vars = makeVars maxBound :: [Variable]

spaced :: String -> Parsec String u ()
spaced op = do
    whiteSpace
    string op
    whiteSpace
    return ()

whiteSpace = many $ char ' '

cnf :: Parsec String u Formula
cnf = emptyAnd <|> (between (char '(') (char ')') $ do
    whiteSpace
    clauses <- C.sepBy clause (spaced "&&")
    whiteSpace
    return $ And clauses)

emptyAnd, emptyOr :: Parsec String u Formula
emptyAnd = do
    string "true"
    return $ And []
emptyOr = do
    string "false"
    return $ Or []

clause :: Parsec String u Formula
clause = emptyOr <|> (between (char '(') (char ')') $ do
    whiteSpace
    lits <- C.sepBy literal (spaced "||")
    whiteSpace
    return $ Or lits)

literal :: Parsec String u Formula
literal = do
    whiteSpace
    lit <- atom <|> negation
    whiteSpace
    return lit

parseCnf :: String -> Formula
parseCnf str = case parse cnf "" str of
    Left e -> error "parse error"
    Right f -> f
