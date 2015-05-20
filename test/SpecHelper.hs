module SpecHelper
    ( module Test.Hspec,
      module Test.QuickCheck,
      OneHundredOrLess(..),
      TenOrLess(..)
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
        depth <- choose (1,4::Int)
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