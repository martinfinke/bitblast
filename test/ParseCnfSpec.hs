module ParseCnfSpec where

import SpecHelper
import Formula
import FormulaSpec -- Arbitrary Formula
import ParseCnf
import NormalForm
import NormalFormSpec -- Arbitrary Canonical


spec :: Spec
spec = do
    describe "parseCnf" $ do
        it "parses any CNF from prettyPrint" $ do
            property $ \canonical -> (parseCnf . prettyPrint . getFormula) canonical `shouldBe` getFormula canonical
