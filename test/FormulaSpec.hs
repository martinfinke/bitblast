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

    describe "Formula Show instance" $ do
        it "shows an Atom correctly" $ do
            show (Atom (var 5)) `shouldBe` "5"

        it "shows a negated literal" $ do
            show (Not $ Atom (var 33)) `shouldBe` "-33"

        it "shows an empty conjunct as true" $ do
            show (And []) `shouldBe` "true"

        it "shows a conjunct with just one term as just this term" $ do
            show (And [Not $ Atom (var 11)]) `shouldBe` "(-11)"

        it "shows a conjunct with two terms" $ do
            show (And [Not $ Atom (var 11), Atom (var 3)]) `shouldBe` "(-11 && 3)"

        it "shows an empty disjunct as false" $ do
            show (Or []) `shouldBe` "false"

        it "shows a disjunct with just one term as just this term" $ do
            show (Or [Atom (var 25)]) `shouldBe` "(25)"

        it "shows a disjunct with three terms" $ do
            show (Or [Not $ Atom (var 11), Atom (var 23), Not $ Atom (var 3)]) `shouldBe` "(-11 || 23 || -3)"

        it "shows an implication" $ do
            show (Implies (Atom (var 4)) (Not $ Atom (var 1))) `shouldBe` "(4 -> -1)"

        it "shows an empty Xor as false" $ do
            show (Xor []) `shouldBe` "false"

        it "shows an Xor with one term as this term" $ do
            show (Xor [Atom (var 2)]) `shouldBe` "(2)"

        it "shows an Xor with three terms" $ do
            show (Xor [Atom (var 2), Not $ Atom (var 1), Atom (var 15)]) `shouldBe` "(2 XOR -1 XOR 15)"

        it "shows equivalence without terms as true" $ do
            show (Equiv []) `shouldBe` "true"

        it "shows equivalence with one term as true" $ do
            show (Equiv [Not $ Atom (var 3)]) `shouldBe` "true"

        it "shows equivalence with two terms" $ do
            show (Equiv [Not $ Atom (var 3), Atom (var 14)]) `shouldBe` "(-3 <=> 14)"

        it "shows equivalence with three terms" $ do
            show (Equiv [Not $ Atom (var 3), Atom (var 14), Not $ Atom (var 25)]) `shouldBe` "(-3 <=> 14 <=> -25)"

    -- TODO: Nested, more complicated formulae