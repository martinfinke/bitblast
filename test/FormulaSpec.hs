module FormulaSpec where

import SpecHelper
import Formula
import TruthTable

spec :: Spec
spec = do
    describe "eval for literals" $ do
        it "is true if the variable is assigned true" $ do
            let formula = Atom (var 1)
            eval allTrue formula `shouldBe` True

        it "is false if a negated variable is assigned true" $ do
            let negated = Not $ Atom (var 0)
            eval allTrue negated `shouldBe` False

        it "evaluates any Atom correctly" $ do
            property $ \variable bool ->
                let assignment = setVariable allFalse variable bool
                in eval assignment (Atom variable) `shouldBe` bool

        it "evaluates any negated Atom correctly" $ do
            property $ \variable bool ->
                let assignment = setVariable allTrue variable bool
                in eval assignment (Not $ Atom variable) `shouldBe` not bool

    describe "eval And" $ do
        it "is true for empty conjuncts" $ do
            eval allFalse (And []) `shouldBe` True

        it "is false for conjuncts with false literals" $ do
            eval allFalse (And [Atom (var 0), Atom (var 5)]) `shouldBe` False

        it "is true for conjuncts with only true literals" $ do
            eval allFalse (And [Not (Atom (var 3)), Not (Atom (var 1))]) `shouldBe` True

    describe "eval Or" $ do
        it "is false for empty disjuncts" $ do
            eval allFalse (Or []) `shouldBe` False

        it "is true if exactly one literal is true" $ do
            eval allFalse (Or [Not (Atom (var 9)), Atom (var 2)]) `shouldBe` True

        it "is true if more than one literal is true" $ do
            eval allFalse (Or [Not (Atom (var 9)), Atom (var 2), Not (Atom (var 1))]) `shouldBe` True

        it "is false if there are no true literals" $ do
            eval allFalse (Or [Atom (var 1), Atom (var 5), Atom (var 19)]) `shouldBe` False

    describe "eval implication" $ do
        it "is true when premise is false" $ do
            let formula = Implies (Atom (var 0)) (Atom (var 13))
            eval allFalse formula `shouldBe` True

        it "is true when premise is true and conclusion is true" $ do
            let formula = Implies (Atom (var 0)) (Atom (var 2))
            eval allTrue formula `shouldBe` True

        it "is false when premise is true and conclusion is false" $ do
            let formula = Implies (Atom (var 0)) (Not (Atom (var 13)))
            eval allTrue formula `shouldBe` False

    describe "eval Xor" $ do
        it "is false for an empty Xor" $ do
            eval allFalse (Xor []) `shouldBe` False

        it "is true for an Xor with exactly one true term" $ do
            eval allFalse (Xor [Not $ Atom (var 3)]) `shouldBe` True

        it "is false for an Xor with two true terms" $ do
            eval allTrue (Xor [Atom (var 2), Atom (var 1)]) `shouldBe` False

        it "is true for an Xor with three true terms" $ do
            eval allTrue (Xor [Atom (var 2), Atom (var 1), Atom (var 1), Not (Atom (var 2))]) `shouldBe` True

    describe "eval Equiv" $ do
        it "is true if there are no terms" $ do
            eval allTrue (Equiv []) `shouldBe` True

        it "is true if there's only one term (no matter if that term is true or false)" $ do
            eval allFalse (Equiv [Atom (var 1)]) `shouldBe` True

        it "is true if all (> 1) terms are false" $ do
            eval allFalse (Equiv [Atom (var 1), Atom (var 15), Atom (var 3)]) `shouldBe` True

        it "is true if all (> 1) terms are true" $ do
            eval allFalse (Equiv [Not $ Atom (var 1), Not $ Atom (var 15), Not $ Atom (var 3)]) `shouldBe` True

        it "is false if one term is false" $ do
            eval allFalse (Equiv [Not $ Atom (var 1), Atom (var 15), Not $ Atom (var 3)]) `shouldBe` False

    -- TODO: Nested, more complicated formulae