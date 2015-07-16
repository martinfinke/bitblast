{-# language FlexibleContexts #-}
module ParseFormula where

import Formula
import Variable

import Text.Parsec
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Combinator as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)

parseFormula :: String -> Formula
parseFormula str = case parse formula "" str of
    Left e -> error "parse error"
    Right f -> f

instance Read Formula where
    readsPrec p s = case parse ((,) <$> formula <*> getInput) "" s of
        Right (f, r) -> [(f, r)]

formula :: Parsec String u Formula
formula = negation
    <|> try land
    <|> try lor
    <|> try xor
    <|> try implies
    <|> try equiv
    <|> atom
    <?> "formula"

negation = do
    minusSign <- char '-'
    a <- formula
    return $ Not a

atom = do
    num <- read <$> many1 digit
    return $ Atom (vars!!fromIntegral num)
    where vars = makeVars maxBound :: [Variable]

land, lor, equiv :: Parsec String u Formula
land = operator "&&" And
lor = operator "||" Or
xor = operator "XOR" Xor
equiv = operator "<=>" Equiv

implies = between (char '(') (char ')') $ do
    premise <- formula
    spaced "->"
    conclusion <- formula
    return $ Implies premise conclusion

operator :: String -> ([Formula] -> Formula) -> Parsec String u Formula
operator str op = between (char '(') (char ')') $ do
    fs <- C.sepBy formula (spaced str)
    return $ op fs

spaced :: String -> Parsec String u ()
spaced op = do
    whiteSpace
    string op
    whiteSpace
    return ()
    where whiteSpace = many $ char ' '
