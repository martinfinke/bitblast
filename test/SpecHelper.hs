module SpecHelper
    ( module Test.Hspec,
      module Test.QuickCheck,
      OneHundredOrLess(..),
      TenOrLess(..),
      IntList(..),
      shouldBeOneOf,
      evaluate
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

data OneHundredOrLess = OneHundredOrLess Int
    deriving (Show)

instance Arbitrary OneHundredOrLess where
    arbitrary = elements $ map OneHundredOrLess [0..100]

data TenOrLess = TenOrLess Int
    deriving (Show)

instance Arbitrary TenOrLess where
    arbitrary = elements $ map TenOrLess [0..10]

data IntList = IntList [Int]
    deriving(Show)

instance Arbitrary IntList where
    arbitrary = do
        TenOrLess len <- arbitrary :: Gen TenOrLess
        fmap IntList $ vectorOf len (arbitrary :: Gen Int)

shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> Expectation
shouldBeOneOf x xs = x `shouldSatisfy` (`elem` xs)
