module SpecHelper
    ( module Test.Hspec,
      module Test.QuickCheck,
      OneHundredOrLess(..),
      TenOrLess(..),
      shouldBeOneOf,
      nestedFormula,
      smallNestedFormula
    ) where

import Test.Hspec
import Test.QuickCheck

import TruthTable
import Formula


data OneHundredOrLess = OneHundredOrLess Int
    deriving (Show)

instance Arbitrary OneHundredOrLess where
    arbitrary = elements $ map OneHundredOrLess [0..100]

data TenOrLess = TenOrLess Int
    deriving (Show)

instance Arbitrary TenOrLess where
    arbitrary = elements $ map TenOrLess [0..10]

instance Arbitrary Variable where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Assignment where
    arbitrary = resize (fromEnum (maxBound::Variable)) $ do
        trueVariables <- listOf1 arbitrary :: Gen [Variable]
        return $ setVariables (zip trueVariables (repeat True)) allFalse

instance Arbitrary OutputValue where
    arbitrary = elements [T,F,DC]

instance Arbitrary TruthTable where
    arbitrary = resize 20 $ do
        (TenOrLess tenOrLess) <- arbitrary
        let numVariables = max 2 tenOrLess
        let empty = emptyTable numVariables
        let boundedAssignmentsGen = elements [minBound..toEnum (numVariables-1)] :: Gen Assignment
        randomAssignments <- listOf1 boundedAssignmentsGen
        randomOutputs <- vector (length randomAssignments) :: Gen [OutputValue]
        let rows = zip randomAssignments randomOutputs
        return $ setOutputs rows empty

instance Arbitrary Formula where
    arbitrary = do
        (TenOrLess tenOrLess) <- arbitrary
        let numVariables = max 2 tenOrLess
        let variables = map var [0..numVariables-1]
        depth <- choose (1,5::Int)
        randomFormula variables depth

randomFormula :: [Variable] -> Int -> Gen Formula
randomFormula variables 0 = do
    variable <- elements variables
    return $ Atom variable
randomFormula variables depth = do
    breadth <- choose (2,5::Int)
    subFormulas <- vectorOf breadth $ randomFormula variables (depth-1)
    operator <- elements [
        Not . head,
        And,
        Or,
        \(f1:f2:_) -> Implies f1 f2,
        Xor,
        Equiv
        ]
    return $ operator subFormulas


shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> Expectation
shouldBeOneOf x xs = x `shouldSatisfy` (`elem` xs)

nestedFormula :: Formula
nestedFormula = Not $ And [Not x3, x1, Implies (Xor [x15, Not x27, Equiv [x3, x2, Or [Not x3], x27]]) (Or [x3, x2])]
    where [x1, x2, x3, x15, x27] = map (Atom . var) [1, 2, 3, 15, 27]

smallNestedFormula :: Formula
smallNestedFormula = Equiv [Xor [Not $ Atom (var 1), Atom (var 0)], Not $ And [Atom (var 1), Or [Not $ Atom (var 0), Atom (var 3)]]]

