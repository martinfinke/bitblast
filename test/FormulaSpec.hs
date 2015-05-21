module FormulaSpec where

import SpecHelper
import Formula
import TruthTable
import qualified Data.Set as Set

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
                let assignment = setVariable variable bool allFalse
                in eval assignment (Atom variable) `shouldBe` bool

        it "evaluates any negated Atom correctly" $ do
            property $ \variable bool ->
                let assignment = setVariable variable bool allTrue
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

    describe "eval nestedFormula" $ do
        it "evaluates nestedFormula correctly" $ do
            let assignment = setVariables [
                    (var 1, True),
                    (var 2, False),
                    (var 3, True),
                    (var 15, False),
                    (var 27, True)
                    ] allFalse
            eval assignment nestedFormula `shouldBe` True

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

        it "shows nestedFormula correctly" $ do
            show nestedFormula `shouldBe` "-(-3 && 1 && ((15 XOR -27 XOR (3 <=> 2 <=> (-3) <=> 27)) -> (3 || 2)))"

    describe "variableSet" $ do
        it "is the singleton set with one variable for an atom" $ do
            variableSet (Atom (var 4)) `shouldBe` Set.fromList [var 4]

        it "is the singleton set with one variable for a negated atom" $ do
            variableSet (Not $ Atom (var 14)) `shouldBe` Set.fromList [var 14]

        it "is the empty set for an empty conjunct" $ do
            variableSet (And []) `shouldBe` Set.empty

        it "works for And with one term" $ do
            variableSet (And [Atom $ var 3]) `shouldBe` Set.fromList [var 3]

        it "works for nestedFormula" $ do
            variableSet nestedFormula `shouldBe` Set.fromList (map var [1, 2, 3, 15, 27])

    describe "numVariablesInFormula" $ do
        it "is 0 for an empty Formula" $ do
            numVariablesInFormula (And []) `shouldBe` 0

        it "is 5 for nestedFormula" $ do
            numVariablesInFormula nestedFormula `shouldBe` 5

    describe "allBoolCombinations" $ do
        it "is the allFalse assignment for no variables" $ do
            allBoolCombinations (Set.fromList []) `shouldBe` [allFalse]

        it "gives two assignments for a set with one variable" $ do
            let variable = var 0
            allBoolCombinations (Set.fromList [variable]) `shouldBe` [
                    allFalse,
                    setVariable variable True allFalse
                ]

        it "gives four assignments for two variables" $ do
            let variables = Set.fromList [var 3, var 15]
            let combinations = allBoolCombinations variables
            combinations `shouldBe` [
                allFalse,
                setVariable (var 3) True allFalse,
                setVariable (var 15) True allFalse,
                setVariable (var 15) True $ setVariable (var 3) True allFalse
                ]

    describe "possibleAssignments" $ do
        it "gives 2^5 = 64 assignments for nestedFormula" $ do
            length (possibleAssignments nestedFormula) `shouldBe` 2^5
    
    describe "toTruthTable" $ do
        it "creates a TruthTable for a single Atom" $ do
            let variable = var 0
            let expectedTable = setOutput (setVariable variable True allFalse) T $ setOutput allFalse F $ emptyTable 1
            toTruthTable (Atom variable) `shouldBe` expectedTable

        it "creates a TruthTable for a negated Atom" $ do
            let variable = var 1
            let expectedTable = setOutput (setVariable variable True allFalse) F $ setOutput allFalse T $ emptyTable 2
            toTruthTable (Not $ Atom variable) `shouldBe` expectedTable

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

    describe "assignmentToMaxterm" $ do
        it "creates a disjunct of a single literal for a singleton set of variables" $ do
            assignmentToMaxterm (Set.fromList [var 3]) allFalse `shouldBe` Or [Atom $ var 3]

        it "creates a disjunct of two literals for a set of two variables" $ do
            assignmentToMaxterm (Set.fromList [var 1, var 3]) allTrue `shouldBe` Or [Not $ Atom (var 1), Not $ Atom (var 3)]

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

nestedFormula :: Formula
nestedFormula = Not $ And [Not x3, x1, Implies (Xor [x15, Not x27, Equiv [x3, x2, Or [Not x3], x27]]) (Or [x3, x2])]
    where [x1, x2, x3, x15, x27] = map (Atom . var) [1, 2, 3, 15, 27]

smallNestedFormula :: Formula
smallNestedFormula = Equiv [Xor [Not $ Atom (var 1), Atom (var 0)], Not $ And [Atom (var 1), Or [Not $ Atom (var 0), Atom (var 3)]]]

