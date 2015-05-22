module QuineMcCluskeySpec where

import SpecHelper
import Control.Exception (evaluate)
import QuineMcCluskey
import TruthTable(var, fromTermNumber)
import Formula(Formula(..))
import NormalForm(assignmentToMinterm, termLiterals)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V

spec :: Spec
spec = do
    let [x0,x1,x2,x3] = map (Atom . var) [0,1,2,3]
    let _0pos3neg = [Not x0, Not x1, Not x2]
    let _1pos2neg = [x0, Not x1, Not x2]
    let _2pos1neg_1diff = [x0, Not x1, x2]
    let _2pos1neg_2diff = [Not x0, x1, x2]
    let _3pos0neg = [x0, x1, x2]

    describe "valueForVariableIndex" $ do
        it "is Nothing for an empty Formula" $ do
            property $ \variableIndex -> valueForVariableIndex (And []) variableIndex `shouldBe` Nothing

        it "is Nothing for a Variable that doesn't appear in the Formula" $ do
            valueForVariableIndex (And _0pos3neg) 3 `shouldBe` Nothing

        it "is True for a Variable that appears as a positive literal" $ do
            valueForVariableIndex (Or _2pos1neg_1diff) 0 `shouldBe` Just True
        it "is False for a Variable that appears as a negative literal" $ do
            valueForVariableIndex (And _1pos2neg) 1 `shouldBe` Just False
            valueForVariableIndex (And _1pos2neg) 2 `shouldBe` Just False

    describe "termToQmcTerm" $ do
        it "creates an empty QmcTerm when the length is 0" $ do
            termToQmcTerm 0 (And []) `shouldBe` QmcTerm V.empty

    describe "QmcTerm Show instance" $ do
        it "shows a term with length 0 as the empty string" $ do
            show (termToQmcTerm 0 (And [])) `shouldBe` ""

        it "shows a term with length 1 as 1" $ do
            show (termToQmcTerm 1 (And [x0])) `shouldBe` "1"

        it "shows a term with length 2 as 10" $ do
            show (termToQmcTerm 2 (And [x1,Not x0])) `shouldBe` "10"

        it "shows a term with length 4 and a Dont-Care as 0-10" $ do
            show (termToQmcTerm 4 (And [Not x3,x1,Not x0])) `shouldBe` "0-10"

    describe "formulaToQmcTerms" $ do
        it "converts an empty Formula to the empty list" $ do
            map show (formulaToQmcTerms (Equiv [])) `shouldBe` []

        it "converts a CNF to QmcTerms" $ do
            let cnf = And [
                    Or _0pos3neg,
                    Or _1pos2neg,
                    Or _2pos1neg_1diff,
                    Or _2pos1neg_2diff,
                    Or _3pos0neg
                    ]
            map show (formulaToQmcTerms cnf) `shouldBe` [
                "000", "001", "101", "110", "111"
                ]

    describe "numRelevantLiterals" $ do
        it "is 0 for a disjunction without negative literals" $ do
            numRelevantLiterals (Or [Atom (var 1), Atom (var 2)]) `shouldBe` 0

        it "is 2 for a conjunction with 2 positive literals" $ do
            numRelevantLiterals (And [Atom (var 2), Atom (var 1), Not $ Atom (var 1)]) `shouldBe` 2

        it "throws an error for a Formula that isn't a conjunction/disjunction of literals" $ do
            evaluate (numRelevantLiterals (Equiv [Atom (var 1)])) `shouldThrow` anyException

    describe "groupByRelevantLiterals" $ do
        it "groups a CNF by number of negative literals" $ do
            let cnf = And [
                    Or [Atom (var 0), Not $ Atom (var 1)],
                    Or [Not $ Atom (var 0), Not $ Atom (var 1)],
                    Or [Not $ Atom (var 0), Atom (var 1)]
                    ]
            groupByRelevantLiterals cnf `shouldBe` Map.fromList [
                (1, [Or [Atom (var 0), Not $ Atom (var 1)], Or [Not $ Atom (var 0), Atom (var 1)]]),
                (2, [Or [Not $ Atom (var 0), Not $ Atom (var 1)]])
                ]

        it "groups a DNF by number of positive literals" $ do
            let dnf = Or [
                    And [Atom (var 0), Not $ Atom (var 1)],
                    And [Not $ Atom (var 0), Not $ Atom (var 1)],
                    And [Not $ Atom (var 0), Atom (var 1)]
                    ]
            groupByRelevantLiterals dnf `shouldBe` Map.fromList [
                (1, [And [Atom (var 0), Not $ Atom (var 1)], And [Not $ Atom (var 0), Atom (var 1)]]),
                (0, [And [Not $ Atom (var 0), Not $ Atom (var 1)]])
                ]

    describe "neighbourKeys" $ do
        it "is the empty list for an empty input list" $ do
            neighbourKeys [] `shouldBe` []

        it "is the empty list for an input list with just one element" $ do
            neighbourKeys [5] `shouldBe` []

        it "is one pair for a list with two neighbours" $ do
            neighbourKeys [3,4] `shouldBe` [(3,4)]

        it "is the empty list for an input list with two numbers that aren't neighbours" $ do
            neighbourKeys [3,5] `shouldBe` []

        it "is two pairs for a list with three adjacent numbers" $ do
            neighbourKeys [6,7,8] `shouldBe` [(6,7), (7,8)]

        it "is two pairs for a list with two neighbouring pairs which are apart from each other" $ do
            neighbourKeys [3,4, 7,8] `shouldBe` [(3,4), (7,8)]

    --describe "hammingDistance" $ do
    --    it "is zero for two empty terms" $ do
    --        hammingDistance (Or [], Or []) `shouldBe` 0

    --    it "is 1 for an empty term and one with 1 literal" $ do
    --        hammingDistance (And [], And [Atom (var 0)]) `shouldBe` 1
    --        hammingDistance (And [Atom (var 0)], And []) `shouldBe` 1


    --    it "is 2 for an empty term and one with 1 literal" $ do
    --        hammingDistance (And [], And [Atom (var 0), Not $ Atom (var 1)]) `shouldBe` 2
    --        hammingDistance (And [Not $ Atom (var 0), Atom (var 0)], And []) `shouldBe` 2

    --    it "is 2 for a term with 1 literal and another with 3 literals" $ do
    --        hammingDistance (And [Atom (var 0)], And [Atom (var 0), Atom (var 1), Not $ Atom (var 0)]) `shouldBe` 2

    --    it "is 1 for a pair of terms with one different literal" $ do
    --        hammingDistance (And _1pos2neg, And _2pos1neg_1diff) `shouldBe` 1
    --        hammingDistance (And _1pos2neg, And _2pos1neg_2diff) `shouldBe` 2
            

    --describe "neighbourTerms" $ do
    --    it "is empty for an empty map" $ do
    --        neighbourTerms Map.empty `shouldBe` []

    --    it "is empty if there are no neighbour terms" $ do
    --        let termMap = groupByRelevantLiterals $ And [Or _0pos3neg, Or _2pos1neg_1diff]
    --        neighbourTerms termMap `shouldBe` []

    --    it "is one pair if there are two neighbour terms" $ do
    --        let termMap = groupByRelevantLiterals $ Or [And _1pos2neg, And _2pos1neg_1diff]
    --        neighbourTerms termMap `shouldBe` [(And _1pos2neg, And _2pos1neg_1diff)]

    --    it "checks the Hamming distance, not just the number of relevant literals" $ do
    --        let termMap = groupByRelevantLiterals $ Or [And _1pos2neg, And _2pos1neg_2diff]
    --        neighbourTerms termMap `shouldBe` []

    --    it "is one pair if the hamming distance is correct" $ do
    --        let termMap = groupByRelevantLiterals $ And [Or _1pos2neg, Or _2pos1neg_1diff]
    --        neighbourTerms termMap `shouldBe` [(Or _2pos1neg_1diff, Or _1pos2neg)]

    --    it "is two pairs if there are two neighbours" $ do
    --        let termMap = groupByRelevantLiterals $ Or [And _0pos3neg, And _1pos2neg, And _2pos1neg_1diff, And _2pos1neg_2diff, And _3pos0neg]
    --        let expected = [
    --                (And _0pos3neg, And _1pos2neg),
    --                (And _1pos2neg, And _2pos1neg_1diff),
    --                (And _2pos1neg_1diff, And _3pos0neg),
    --                (And _2pos1neg_2diff, And _3pos0neg)
    --                ]
    --        neighbourTerms termMap `shouldBe` expected

    describe "mergeTerms" $ do
        it "merges two identical terms to that same term" $ do
            mergeTerms (Or _1pos2neg, Or _1pos2neg) `shouldBe` Or _1pos2neg

        it "merges two disjunct terms to an empty term" $ do
            mergeTerms (And _1pos2neg, And _2pos1neg_2diff) `shouldBe` And []

        it "merges two terms with exactly one difference to a new term with everything they had in common" $ do
            mergeTerms (Or _1pos2neg, Or _2pos1neg_1diff) `shouldBe` Or [x0, Not x1]
            mergeTerms (And _3pos0neg, And _2pos1neg_1diff) `shouldBe` And [x0, x2]
            mergeTerms (Or _3pos0neg, Or _2pos1neg_2diff) `shouldBe` Or [x1, x2]

    describe "partitionTerms" $ do
        it "partitions an empty Map into two empty lists" $ do
            let termMap = groupByRelevantLiterals $ And []
            partitionTerms termMap `shouldBe` ([], [])

    describe "Mikhelson example" $ do
        -- https://www.youtube.com/watch?v=G9_oICLaLBU
        let mikhelsonExample@(Or [mik2, mik5, mik6, mik11, mik12, mik14, mik15]) = Or [
                And [Not x0, x1, Not x2, Not x3], -- 2
                And [x0, Not x1, x2, Not x3], -- 5
                And [Not x0, x1, x2, Not x3], -- 6
                And [x0, x1, Not x2, x3], -- 11
                And [Not x0, Not x1, x2, x3], -- 12
                And [Not x0, x1, x2, x3], -- 14
                And [x0, x1, x2, x3] -- 15
                ]

        it "groups by relevant literals as in the video" $ do
            let expected = Map.fromList [
                    (1, [mik2]),
                    (2, [mik5, mik6, mik12]),
                    (3, [mik11, mik14]),
                    (4, [mik15])
                    ]
            groupByRelevantLiterals mikhelsonExample `shouldBe` expected

    describe "quineMinterms/quineMaxterms" $ do
        it "yields no terms for no variables" $ do
            quineMinterms 0 `shouldBe` []

        it "gives a positive and negated literal for one variable" $ do
            quineMinterms 1 `shouldBe` [And [Not $ Atom $ var 0], And [Atom $ var 0]]

    describe "Lesson 10 example" $ do
        let [t0,t1,t2,t3,t4,t5,t6,t7] = quineMinterms 3
        let onSet = [t2,t4,t5,t6,t7]
        it "groups by relevant literals as in the video" $ do
            let expected = Map.fromList [
                    (1, [t2,t4]),
                    (2, [t5,t6]),
                    (3, [t7])
                    ]
            groupByRelevantLiterals (Or onSet) `shouldBe` expected

        --it "partitions the terms as in the video" $ do
        --    let initialTermMap = groupByRelevantLiterals $ Or onSet
        --    let afterFirstStep = partitionTerms initialTermMap
        --    afterFirstStep `shouldBe` ([], map mergeTerms [
        --                               (t2,t6),
        --                               (t4,t5),
        --                               (t4,t6),
        --                               (t5,t7),
        --                               (t6,t7)
        --                               ])
        --    let termMapAfterFirstStep = groupByRelevantLiterals $ Or (snd afterFirstStep)
        --    let afterSecondStep = partitionTerms termMapAfterFirstStep
        --    pending
            --afterFirstStep `shouldBe` ([],[])
