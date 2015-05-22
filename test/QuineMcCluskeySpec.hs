module QuineMcCluskeySpec where

import SpecHelper
import QuineMcCluskey
import TruthTable(var)
import Formula(Formula(..))
import NormalForm(FormType(..))
import qualified Data.Vector.Unboxed as V


instance Arbitrary QmcTerm where
    arbitrary = do
        (TenOrLess len) <- arbitrary
        str <- vectorOf len $ elements "10-"
        return $ fromString str

spec :: Spec
spec = do
    let [x0,x1,x2,x3] = map (Atom . var) [0,1,2,3]
    let _0pos3neg = [Not x0, Not x1, Not x2]
    let _1pos2neg = [x0, Not x1, Not x2]
    let _2pos1neg_1diff = [x0, Not x1, x2]
    let _2pos1neg_2diff = [Not x0, x1, x2]
    let _3pos0neg = [x0, x1, x2]
    let testCnf = And [
            Or _0pos3neg,
            Or _1pos2neg,
            Or _2pos1neg_1diff,
            Or _2pos1neg_2diff,
            Or _3pos0neg
            ]

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
        it "is inverse to fromString" $ do
            property $ \qmcTerm -> (fromString . show) qmcTerm `shouldBe` qmcTerm

    describe "formulaToQmcTerms" $ do
        it "converts an empty Formula to the empty list" $ do
            map show (formulaToQmcTerms (Equiv [])) `shouldBe` []

        it "converts a CNF to QmcTerms" $ do
            map show (formulaToQmcTerms testCnf) `shouldBe` [
                "000", "001", "101", "110", "111"
                ]

    describe "numRelevantLiterals" $ do
        it "is 0 for an empty QmcTerm" $ do
            let empty = fromString ""
            numRelevantLiterals' CNFType empty `shouldBe` 0
            numRelevantLiterals' DNFType empty `shouldBe` 0

        it "calculates the correct number of literals" $ do
            let qmcTerms = formulaToQmcTerms testCnf
            map (numRelevantLiterals' CNFType) qmcTerms `shouldBe` [3,2,1,1,0]
            map (numRelevantLiterals' DNFType) qmcTerms `shouldBe` [0,1,2,2,3]

        it "works correctly with don't care positions" $ do
            let qmcTerm = fromString "0-1-110----"
            numRelevantLiterals' CNFType qmcTerm `shouldBe` 2
            numRelevantLiterals' DNFType qmcTerm `shouldBe` 3

    describe "hammingDistance" $ do
        it "is zero for two empty terms" $ do
            hammingDistance (fromString "", fromString "") `shouldBe` 0

        it "is 1 for an empty term and one with 1 literal" $ do
            hammingDistance (fromString "-", fromString "1") `shouldBe` 1
            hammingDistance (fromString "1", fromString "-") `shouldBe` 1

        it "is 2 for an empty term and one with 2 literals" $ do
            hammingDistance (fromString "--", fromString "01") `shouldBe` 2
            hammingDistance (fromString "00", fromString "--") `shouldBe` 2

        it "is 2 for a term with 1 literal and another with 3 literals" $ do
            hammingDistance (fromString "-1-", fromString "110") `shouldBe` 2

        it "is 1 for a pair of terms with one different literal" $ do
            hammingDistance (fromString "000--01--", fromString "010--01--") `shouldBe` 1

    describe "dashesLineUp" $ do
        it "is False if dashes don't line up" $ do
            dashesLineUp (fromString "-1--") (fromString "----") `shouldBe` False

        it "is True if dashes line up" $ do
            dashesLineUp (fromString "-0---") (fromString "-1---") `shouldBe` True

        it "is True for empty terms" $ do
            dashesLineUp (fromString "") (fromString "") `shouldBe` True

    describe "dashWhenDifferent" $ do
        it "outputs a dash (Nothing) if elements are different" $ do
            dashWhenDifferent (Just True) (Just False) `shouldBe` Nothing
            dashWhenDifferent (Just False) (Just True) `shouldBe` Nothing

        it "outputs the Bool value if elements are the same" $ do
            dashWhenDifferent (Just True) (Just True) `shouldBe` Just True
            dashWhenDifferent (Just False) (Just False) `shouldBe` Just False

        it "throws an error when dashes don't align" $ do
            evaluate (dashWhenDifferent Nothing (Just True)) `shouldThrow` anyErrorCall
            evaluate (dashWhenDifferent (Just False) Nothing) `shouldThrow` anyErrorCall

    describe "mergeTerms" $ do
        it "doesn't merge two terms that are equal" $ do
            mergeTerms (fromString "101") (fromString "101") `shouldBe` Nothing

        it "doesn't merge two terms where the dashes don't line up" $ do
            mergeTerms (fromString "--1") (fromString "-11") `shouldBe` Nothing
            mergeTerms (fromString "01-") (fromString "-0-") `shouldBe` Nothing

        it "throws an error for terms with different lengths" $ do
            evaluate (mergeTerms (fromString "-") (fromString "-1")) `shouldThrow` anyErrorCall

        it "doesn't merge two terms that are different in 2 positions" $ do
            mergeTerms (fromString "-10") (fromString "-01") `shouldBe` Nothing

        it "doesn't merge two terms that are different in 3 positions" $ do
            mergeTerms (fromString "-100") (fromString "-011") `shouldBe` Nothing

        it "merges two terms that have exactly 1 difference" $ do
            mergeTerms (fromString "1") (fromString "0") `shouldBe` Just (fromString "-")
            mergeTerms (fromString "10") (fromString "11") `shouldBe` Just (fromString "1-")
            mergeTerms (fromString "1-0") (fromString "1-1") `shouldBe` Just (fromString "1--")
            mergeTerms (fromString "--10-110-1") (fromString "--10-100-1") `shouldBe` Just (fromString "--10-1-0-1")
            

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

    
            
