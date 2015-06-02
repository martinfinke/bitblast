module FormulaSpec where

import SpecHelper
import Formula
import Variable hiding (eval)
import qualified Variable as V
import VariableSpec
import TruthTableSpec
import qualified Data.Set as Set
import Control.Monad(forM)

instance Arbitrary Formula where
    arbitrary = do
        (TenOrLess tenOrLess) <- arbitrary
        let numVariables = max 2 tenOrLess
        variables <- randomVariables numVariables
        depth <- choose (1,3::Int)
        randomFormula variables depth

randomFormula :: [Variable] -> Int -> Gen Formula
randomFormula variables 0 = do
    variable <- elements variables
    return $ Atom variable
randomFormula variables depth = do
    breadth <- choose (2,4::Int)
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




spec :: Spec
spec = do
    let [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9] = V.eval initial $ do
            forM [0..9] $ \i -> var ('x' : show i)

    let nestedFormula = Not $ And [Not $ Atom x3, Atom x1, Implies (Xor [Atom x7, Not $ Atom x9, Equiv [Atom x3, Atom x2, Or [Not $ Atom x3], Atom x9]]) (Or [Atom x3, Atom x2])]

    let smallNestedFormula = Equiv [Xor [Not $ Atom x1, Atom x0], Not $ And [Atom x1, Or [Not $ Atom x0, Atom x3]]]

    let allFalseFor = allFalse . variableSet
    let allTrueFor = allTrue . variableSet


    describe "eval for literals" $ do
        it "is true if the variable is assigned true" $ do
            let formula = Atom x1
            eval (allTrue (variableSet formula)) formula `shouldBe` True

        it "is false if a negated variable is assigned true" $ do
            let negated = Not $ Atom x0
            eval (allTrue (variableSet negated)) negated `shouldBe` False

        it "evaluates any Atom correctly" $ do
            property $ \variable bool ->
                let assignment = setVar variable bool $ allFalse (Set.fromList [variable])
                in eval assignment (Atom variable) `shouldBe` bool

        it "evaluates any negated Atom correctly" $ do
            property $ \variable bool ->
                let assignment = setVar variable bool $ allTrue (Set.fromList [variable])
                in eval assignment (Not $ Atom variable) `shouldBe` not bool

    describe "eval And" $ do
        it "is true for empty conjuncts" $ do
            eval (allFalse Set.empty) (And []) `shouldBe` True

        it "is false for conjuncts with false literals" $ do
            let f = And [Atom (x0), Atom (x5)]
            eval (allFalseFor f) f `shouldBe` False

        it "is true for conjuncts with only true literals" $ do
            let f = And [Not (Atom (x3)), Not (Atom (x1))]
            eval (allFalseFor f) f `shouldBe` True

    describe "eval Or" $ do
        it "is false for empty disjuncts" $ do
            let f = Or []
            eval (allFalseFor f) f `shouldBe` False

        it "is true if exactly one literal is true" $ do
            let f = Or [Not (Atom (x9)), Atom (x2)]
            eval (allFalseFor f) f `shouldBe` True

        it "is true if more than one literal is true" $ do
            let f = Or [Not (Atom (x9)), Atom (x2), Not (Atom (x1))]
            eval (allFalseFor f) f `shouldBe` True

        it "is false if there are no true literals" $ do
            let f = Or [Atom (x1), Atom (x5), Atom x9]
            eval (allFalseFor f) f `shouldBe` False

    describe "eval implication" $ do
        it "is true when premise is false" $ do
            let f = Implies (Atom (x0)) (Atom x8)
            eval (allFalseFor f) f `shouldBe` True

        it "is true when premise is true and conclusion is true" $ do
            let f = Implies (Atom (x0)) (Atom (x2))
            eval (allTrueFor f) f `shouldBe` True

        it "is false when premise is true and conclusion is false" $ do
            let f = Implies (Atom (x0)) (Not (Atom x8))
            eval (allTrueFor f) f `shouldBe` False

    describe "eval Xor" $ do
        it "is false for an empty Xor" $ do
            let f = Xor []
            eval (allFalseFor f) f `shouldBe` False

        it "is true for an Xor with exactly one true term" $ do
            let f = Xor [Not $ Atom (x3)]
            eval (allFalseFor f) f `shouldBe` True

        it "is false for an Xor with two true terms" $ do
            let f = Xor [Atom (x2), Atom (x1)]
            eval (allTrueFor f) f `shouldBe` False

        it "is true for an Xor with three true terms" $ do
            let f = Xor [Atom (x2), Atom (x1), Atom (x1), Not (Atom (x2))]
            eval (allTrueFor f) f `shouldBe` True

    describe "eval Equiv" $ do
        it "is true if there are no terms" $ do
            let f = Equiv []
            eval (allTrueFor f) f `shouldBe` True

        it "is true if there's only one term (no matter if that term is true or false)" $ do
            let f = Equiv [Atom (x1)]
            eval (allFalseFor f) f `shouldBe` True

        it "is true if all (> 1) terms are false" $ do
            let f = Equiv [Atom (x1), Atom (x9), Atom (x3)]
            eval (allFalseFor f) f `shouldBe` True

        it "is true if all (> 1) terms are true" $ do
            let f = Equiv [Not $ Atom (x1), Not $ Atom x9, Not $ Atom (x3)]
            eval (allFalseFor f) f `shouldBe` True

        it "is false if one term is false" $ do
            let f = Equiv [Not $ Atom (x1), Atom x9, Not $ Atom (x3)]
            eval (allFalseFor f) f `shouldBe` False

    describe "eval nestedFormula" $ do
        it "evaluates nestedFormula correctly" $ do
            let assignment = assignmentFromList [
                    (x1, True),
                    (x2, False),
                    (x3, True),
                    (x7, False),
                    (x9, True)
                    ]
            eval assignment nestedFormula `shouldBe` True

    describe "Formula Show instance" $ do
        it "shows an Atom correctly" $ do
            show (Atom x5) `shouldBe` "5"

        it "shows a negated literal" $ do
            show (Not $ Atom x7) `shouldBe` "-7"

        it "shows an empty conjunct as true" $ do
            show (And []) `shouldBe` "true"

        it "shows a conjunct with just one term as just this term" $ do
            show (And [Not $ Atom (x9)]) `shouldBe` "(-9)"

        it "shows a conjunct with two terms" $ do
            show (And [Not $ Atom (x6), Atom (x3)]) `shouldBe` "(-6 && 3)"

        it "shows an empty disjunct as false" $ do
            show (Or []) `shouldBe` "false"

        it "shows a disjunct with just one term as just this term" $ do
            show (Or [Atom (x4)]) `shouldBe` "(4)"

        it "shows a disjunct with three terms" $ do
            show (Or [Not $ Atom (x7), Atom (x9), Not $ Atom (x3)]) `shouldBe` "(-7 || 9 || -3)"

        it "shows an implication" $ do
            show (Implies (Atom (x4)) (Not $ Atom (x1))) `shouldBe` "(4 -> -1)"

        it "shows an empty Xor as false" $ do
            show (Xor []) `shouldBe` "false"

        it "shows an Xor with one term as this term" $ do
            show (Xor [Atom (x2)]) `shouldBe` "(2)"

        it "shows an Xor with three terms" $ do
            show (Xor [Atom (x2), Not $ Atom (x1), Atom (x9)]) `shouldBe` "(2 XOR -1 XOR 9)"

        it "shows equivalence without terms as true" $ do
            show (Equiv []) `shouldBe` "true"

        it "shows equivalence with one term as true" $ do
            show (Equiv [Not $ Atom (x3)]) `shouldBe` "true"

        it "shows equivalence with two terms" $ do
            show (Equiv [Not $ Atom (x3), Atom (x4)]) `shouldBe` "(-3 <=> 4)"

        it "shows equivalence with three terms" $ do
            show (Equiv [Not $ Atom (x3), Atom (x4), Not $ Atom (x8)]) `shouldBe` "(-3 <=> 4 <=> -8)"

        it "shows nestedFormula correctly" $ do
            show nestedFormula `shouldBe` "-(-3 && 1 && ((7 XOR -9 XOR (3 <=> 2 <=> (-3) <=> 9)) -> (3 || 2)))"

    describe "variableSet" $ do
        it "is the singleton set with one variable for an atom" $ do
            variableSet (Atom (x4)) `shouldBe` Set.fromList [x4]

        it "is the singleton set with one variable for a negated atom" $ do
            variableSet (Not $ Atom (x7)) `shouldBe` Set.fromList [x7]

        it "is the empty set for an empty conjunct" $ do
            variableSet (And []) `shouldBe` Set.empty

        it "works for And with one term" $ do
            variableSet (And [Atom $ x3]) `shouldBe` Set.fromList [x3]

        it "works for nestedFormula" $ do
            variableSet nestedFormula `shouldBe` Set.fromList ([x1, x2, x3, x7, x9])

    describe "allBoolCombinations" $ do
        it "is the allFalse assignment for no variables" $ do
            allBoolCombinations (Set.fromList []) `shouldBe` [allFalse Set.empty]

        it "gives two assignments for a set with one variable" $ do
            let variable = x0
            allBoolCombinations (Set.fromList [variable]) `shouldBe` [
                    allFalseFor $ Atom variable,
                    setVar variable True $ allFalseFor $ Atom variable
                ]

        it "gives four assignments for two variables" $ do
            let variables = Set.fromList [x3, x9]
            let allFalse' = allFalse variables
            let combinations = allBoolCombinations variables
            combinations `shouldBe` [
                allFalse',
                setVar (x3) True allFalse',
                setVar (x9) True allFalse',
                setVar (x9) True $ setVar (x3) True allFalse'
                ]

    describe "possibleAssignments" $ do
        it "gives 2^5 = 64 assignments for nestedFormula" $ do
            length (possibleAssignments nestedFormula) `shouldBe` 2^5
    
    describe "toTruthTable" $ do
        let allFalse' = allFalseFor $ Atom x1
        it "creates a TruthTable for a single Atom" $ do
            let expectedTable = setRow (setVar x1 True allFalse') True $ setRow allFalse' False emptyTable
            toTruthTable (Atom x1) `shouldBe` expectedTable

        it "creates a TruthTable for a negated Atom" $ do
            let expectedTable = setRow (setVar x1 True allFalse') False $ setRow allFalse' True emptyTable
            toTruthTable (Not $ Atom x1) `shouldBe` expectedTable

