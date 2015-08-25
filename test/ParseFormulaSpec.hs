module ParseFormulaSpec where

import SpecHelper
import Formula
import FormulaSpec -- Arbitrary Formula
import ParseFormula


spec :: Spec
spec = do
    describe "parseFormula" $ do
        it "parses any Formula from prettyPrint" $ do
            property $ \f -> parseFormula (prettyPrint f) `shouldBe` f
    describe "Formula Read instance" $ do
        it "is inverse to prettyPrint" $ do
            property $ \f -> read (prettyPrint f) `shouldBe` (f::Formula)
