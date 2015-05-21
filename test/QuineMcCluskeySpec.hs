module QuineMcCluskeySpec where

import SpecHelper
import Control.Exception (evaluate)
import QuineMcCluskey
import TruthTable(var)
import Formula(Formula(..))
import qualified Data.Map.Lazy as Map

spec :: Spec
spec = do
    describe "numRelevantLiterals" $ do
        it "is 0 for a Maxterm without negative literals" $ do
            numRelevantLiterals (Or [Atom (var 1), Atom (var 2)]) `shouldBe` 0

        it "is 2 for a Minterm with 2 positive literals" $ do
            numRelevantLiterals (And [Atom (var 2), Atom (var 1), Not $ Atom (var 1)]) `shouldBe` 2

        it "throws an error for a Formula that isn't a min/maxterm" $ do
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

    describe "neighbourTerms" $ do
        let [x0,x1,x2] = map (Atom . var) [0,1,2]
        let _0pos3neg = [Not x0, Not x1, Not x2]
        let _1pos2neg = [x0, Not x1, Not x2]
        let _2pos1neg_1diff = [x0, Not x1, x2]
        let _2pos1neg_2diff = [Not x0, x1, x2]
        let _3pos0neg = [x0, x1, x2]

        it "is empty for an empty map" $ do
            neighbourTerms Map.empty `shouldBe` []

        it "is empty if there are no neighbour terms" $ do
            let termMap = groupByRelevantLiterals $ And [Or _0pos3neg, Or _2pos1neg_1diff]
            neighbourTerms termMap `shouldBe` []

        it "is one pair if there are two neighbour terms" $ do
            let termMap = groupByRelevantLiterals $ Or [And _1pos2neg, And _2pos1neg_1diff]
            neighbourTerms termMap `shouldBe` [(And _1pos2neg, And _2pos1neg_1diff)]

        it "doesn't care about the actual Hamming distance, only about the number of pos/neg literals" $ do
            let termMap = groupByRelevantLiterals $ Or [And _1pos2neg, And _2pos1neg_2diff]
            neighbourTerms termMap `shouldBe` [(And _1pos2neg, And _2pos1neg_2diff)]