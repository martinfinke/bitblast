module Variable(
                Variable(..),
                var,
                prettyPrint,
                makeVars,
                newVariables
                ) where

import qualified Data.Set as Set

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

newVariables :: Set.Set Variable -> [Variable]
newVariables varSet
    | Set.null varSet = [head (makeVars 1) ..]
    | otherwise = [succ (Set.findMax varSet)..]
