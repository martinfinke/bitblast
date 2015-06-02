module NormalFormSpec(NormalFormSpec.spec) where

import SpecHelper
import qualified Variable as V
import VariableSpec
import Formula(Formula(..), isModelOf, variableSet)
import NormalForm
import qualified Data.Set as Set
import FormulaSpec
import Control.Monad(forM)

instance Arbitrary Canonical where
    arbitrary = do
        numVariables <- choose (1,4)
        variables <- randomVariables numVariables
        rndFormula <- randomFormula variables 2
        toCanonical <- elements [toCanonicalCnf, toCanonicalDnf]
        return $ toCanonical rndFormula


spec :: Spec
spec = do
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = fst $ V.generateVars 10

    let nestedFormula = Not $ And [Not $ Atom x3, Atom x1, Implies (Xor [Atom x7, Not $ Atom x9, Equiv [Atom x3, Atom x2, Or [Not $ Atom x3], Atom x9]]) (Or [Atom x3, Atom x2])]

    let smallNestedFormula = Equiv [Xor [Not $ Atom x1, Atom x0], Not $ And [Atom x1, Or [Not $ Atom x0, Atom x3]]]
    describe "toCanonicalCnf" $ do
        it "creates a CNF for a literal" $ do
            let formula = Atom x9
            getFormula (toCanonicalCnf formula) `shouldBe` And [Or [Atom x9]]

        it "creates an identical CNF for a canonical CNF" $ do
            let formula = And [Or [Atom x1, Not $ Atom x2, Atom x3], Or [Not $ Atom x1, Atom x2, Not $ Atom x3]]
            getFormula (toCanonicalCnf formula) `shouldBe` formula

        it "doesn't change anything when converting a canonical CNF to a canonical CNF" $ do
            let alreadyCnf = getFormula $ toCanonicalCnf smallNestedFormula
            getFormula (toCanonicalCnf alreadyCnf) `shouldBe` alreadyCnf

        it "creates CNFs that are equivalent (for one random assignment) to the original formula" $ do
            property $ \formula assignment ->
                let cnf = getFormula $ toCanonicalCnf formula
                    a1 = V.expandOrReduce False (variableSet formula) assignment
                    a2 = V.expandOrReduce False (variableSet cnf) assignment
                in a2 `isModelOf` cnf `shouldBe` a1 `isModelOf` formula

        it "can create a CNF for a formula without variables" $ do
            let formula = Or [Implies (And []) (And []), Xor []]
            let cnf = toCanonicalCnf formula
            getFormula cnf `shouldBe` And []

    describe "toCanonicalDnf" $ do
        it "creates a DNF for a literal" $ do
            let formula = Atom x1
            getFormula (toCanonicalDnf formula) `shouldBe` Or [And [Atom x1]]

        it "doesn't change anything when converting a canonical DNF to a canonical DNF" $ do
            let alreadyDnf = getFormula $ toCanonicalDnf smallNestedFormula
            getFormula (toCanonicalDnf alreadyDnf) `shouldBe` alreadyDnf

        it "yields the original CNF (or a false CNF) when converting a CNF to DNF and back" $ do
            property $ \formula ->
                let cnf = getFormula $ toCanonicalCnf formula
                    dnf = getFormula $ toCanonicalDnf cnf
                    cnf' = getFormula $ toCanonicalCnf dnf
                in cnf' `shouldBeOneOf` [cnf, And [Or []]]

        it "creates DNFs that are equivalent (for one random assignment) to the original formula" $ do
            property $ \formula assignment ->
                let dnf = getFormula $ toCanonicalDnf formula
                    a1 = V.expandOrReduce False (variableSet formula) assignment
                    a2 = V.expandOrReduce False (variableSet dnf) assignment
                in a2 `isModelOf` dnf `shouldBe` a1 `isModelOf` formula

    describe "isCnf" $ do
        it "is True for a CNF" $ do
            let cnf = getFormula $ toCanonicalCnf smallNestedFormula
            isCnf cnf `shouldBe` True

        it "is False for a DNF" $ do
            let dnf = getFormula $ toCanonicalDnf smallNestedFormula
            isCnf dnf `shouldBe` False

        it "is False for a non-CNF formula" $ do
            isCnf smallNestedFormula `shouldBe` False

    describe "isDnf" $ do
        it "is True for a DNF" $ do
            let dnf = getFormula $ toCanonicalDnf smallNestedFormula
            isDnf dnf `shouldBe` True

        it "is False for a CNF" $ do
            let cnf = getFormula $ toCanonicalCnf smallNestedFormula
            isDnf cnf `shouldBe` False

        it "is False for a non-DNF formula" $ do
            isDnf smallNestedFormula `shouldBe` False

    describe "isLiteral" $ do
        it "is True for a literal" $ do
            isLiteral (Atom x3) `shouldBe` True

        it "is True for a negated literal" $ do
            isLiteral (Not $ Atom x9) `shouldBe` True

        it "is False for a conjunct" $ do
            isLiteral (And []) `shouldBe` False

    describe "isCanonical" $ do
        it "is True for a canonical DNF" $ do
            property $ \formula ->
                let dnf = getFormula $ toCanonicalDnf formula
                in isCanonical dnf `shouldBe` True

        it "is False for a non-canonical CNF" $ do
            let nonCanonical = And [Or [Atom x0, Not $ Atom x1], Or [Atom x1]]
            isCanonical nonCanonical `shouldBe` False

        it "is False for a DNF with a clause containing two literals of one variable" $ do
            let nonCanonical = Or [And [Atom x0, Atom x0]]
            isCanonical nonCanonical `shouldBe` False

        it "is False for a Formula that isn't a CNF/DNF" $ do
            isCanonical nestedFormula `shouldBe` False
            isCanonical smallNestedFormula `shouldBe` False

    describe "ensureCanonical" $ do
        it "makes any formula canonical" $ do
            property $ \formula ->
                let canonical = getFormula $ ensureCanonical formula
                in isCanonical canonical `shouldBe` True

    describe "getStats" $ do
        let clauses </> literals = FormulaStats {numClauses=clauses,numLiterals=literals}
        it "returns 0 clauses and 0 literals for an empty CNF" $ do
            getStats (And []) `shouldBe` 0 </> 0
        it "returns 1 clause and 0 literals for a CNF with one empty clause" $ do
            getStats (And [Or []]) `shouldBe` 1 </> 0
        it "returns 1 clause and 1 literal for a CNF with one clause containing one literal" $ do
            getStats (And [Or [Atom x0]]) `shouldBe` 1 </> 1

        it "returns 2 clauses and 3 literals for a CNF where one variable appears 3 times in 2 clauses" $ do
            getStats (And [Or [Atom x0], Or [Not $ Atom x0, Atom x0]]) `shouldBe` 2 </> 3
