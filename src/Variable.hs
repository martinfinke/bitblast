module Variable(
                Variable(..),
                var,
                prettyPrint,
                makeVars
                ) where

newtype Variable = Variable Int
    deriving(Eq, Ord, Show)

prettyPrint :: Variable -> String
prettyPrint (Variable i) = show i

instance Enum Variable where
    fromEnum (Variable i) = i
    toEnum = var

var :: Int -> Variable
var = Variable

makeVars :: Int -- ^ How many 'Variable's to create
             -> [Variable]
makeVars numvars = map var [0..numvars - 1]
