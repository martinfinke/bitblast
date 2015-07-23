module ParseFormulaSpec where

import SpecHelper
import Formula
import FormulaSpec -- Arbitrary Formula
import ParseFormula


spec :: Spec
spec = do
    describe "parseFormula" $ do
        it "parses any Formula from the Formula Show instance" $ do
            property $ \f -> parseFormula (show f) `shouldBe` f
    describe "Formula Read instance" $ do
        it "is inverse to show" $ do
            property $ \f -> read (show f) `shouldBe` (f::Formula)