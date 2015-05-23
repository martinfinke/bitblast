module NormalFormSpec where

import SpecHelper
import TruthTable(var, allFalse, allTrue)
import Formula(Formula(..), eval)
import NormalForm
import qualified Data.Set as Set
import FormulaSpec

instance Arbitrary FormType where
    arbitrary = elements [CNFType, DNFType]


newtype CanonicalNormalForm = CanonicalNormalForm Formula

instance Show CanonicalNormalForm where
    show (CanonicalNormalForm formula) = show formula

instance Arbitrary CanonicalNormalForm where
    arbitrary = do
        numVariables <- choose (1,4)
        let variables = map var [0..numVariables-1]
        rndFormula <- randomFormula variables 2
        formType <- arbitrary
        return $ CanonicalNormalForm $ case formType of
            CNFType -> toCanonicalCnf rndFormula
            DNFType -> toCanonicalDnf rndFormula


spec :: Spec
spec = do
    describe "toCanonicalCnf" $ do
        it "creates a CNF for a literal" $ do
            let formula = Atom (var 14)
            toCanonicalCnf formula `shouldBe` And [Or [Atom (var 14)]]

        it "creates an identical CNF for a canonical CNF" $ do
            let formula = And [Or [Atom (var 1), Not $ Atom (var 2), Atom (var 3)], Or [Not $ Atom (var 1), Atom (var 2), Not $ Atom (var 3)]]
            toCanonicalCnf formula `shouldBe` formula

        it "doesn't change anything when converting a canonical CNF to a canonical CNF" $ do
            let alreadyCnf = toCanonicalCnf smallNestedFormula
            toCanonicalCnf alreadyCnf `shouldBe` alreadyCnf

        it "creates CNFs that are equivalent (for one random assignment) to the original formula" $ do
            property $ \formula assignment ->
                let cnf = toCanonicalCnf formula
                in eval assignment cnf `shouldBe` eval assignment formula

        it "can create a CNF for a formula without variables" $ do
            let formula = Or [Implies (And []) (And []), Xor []]
            let cnf = toCanonicalCnf formula
            cnf `shouldBe` And []

    describe "toCanonicalDnf" $ do
        it "creates a DNF for a literal" $ do
            let formula = Atom (var 1)
            toCanonicalDnf formula `shouldBe` Or [And [Atom (var 1)]]

        it "doesn't change anything when converting a canonical DNF to a canonical DNF" $ do
            let alreadyDnf = toCanonicalDnf smallNestedFormula
            toCanonicalDnf alreadyDnf `shouldBe` alreadyDnf

        it "yields the original CNF (or a false CNF) when converting a CNF to DNF and back" $ do
            property $ \formula ->
                let cnf = toCanonicalCnf formula
                    dnf = toCanonicalDnf cnf
                    cnf' = toCanonicalCnf dnf
                in cnf' `shouldBeOneOf` [cnf, And [Or []]]

        it "creates DNFs that are equivalent (for one random assignment) to the original formula" $ do
            property $ \formula assignment ->
                let dnf = toCanonicalDnf formula
                in eval assignment dnf `shouldBe` eval assignment formula

    describe "isCnf" $ do
        it "is True for a CNF" $ do
            let cnf = toCanonicalCnf smallNestedFormula
            isCnf cnf `shouldBe` True

        it "is False for a DNF" $ do
            let dnf = toCanonicalDnf smallNestedFormula
            isCnf dnf `shouldBe` False

        it "is False for a non-CNF formula" $ do
            isCnf smallNestedFormula `shouldBe` False

    describe "isDnf" $ do
        it "is True for a DNF" $ do
            let dnf = toCanonicalDnf smallNestedFormula
            isDnf dnf `shouldBe` True

        it "is False for a CNF" $ do
            let cnf = toCanonicalCnf smallNestedFormula
            isDnf cnf `shouldBe` False

        it "is False for a non-DNF formula" $ do
            isDnf smallNestedFormula `shouldBe` False

    describe "isLiteral" $ do
        it "is True for a literal" $ do
            isLiteral (Atom (var 3)) `shouldBe` True

        it "is True for a negated literal" $ do
            isLiteral (Not $ Atom (var 15)) `shouldBe` True

        it "is False for a conjunct" $ do
            isLiteral (And []) `shouldBe` False

    describe "isCanonical" $ do
        it "is True for a canonical DNF" $ do
            property $ \formula ->
                let dnf = toCanonicalDnf formula
                in isCanonical dnf `shouldBe` True

        it "is False for a non-canonical CNF" $ do
            let nonCanonical = And [Or [Atom (var 0), Not $ Atom (var 1)], Or [Atom (var 1)]]
            isCanonical nonCanonical `shouldBe` False

        it "is False for a DNF with a clause containing two literals of one variable" $ do
            let nonCanonical = Or [And [Atom (var 0), Atom (var 0)]]
            isCanonical nonCanonical `shouldBe` False

        it "is False for a Formula that isn't a CNF/DNF" $ do
            isCanonical nestedFormula `shouldBe` False
            isCanonical smallNestedFormula `shouldBe` False

    describe "ensureCanonical" $ do
        it "makes any formula canonical" $ do
            property $ \formula ->
                let canonical = ensureCanonical formula
                in isCanonical canonical `shouldBe` True