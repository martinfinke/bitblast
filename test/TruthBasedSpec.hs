module TruthBasedSpec where

import SpecHelper
import TruthBased

spec :: Spec
spec = do
    let cls = Clause . map Lit
    describe "covers" $ do
        it "is True for the empty clause and an empty assignment" $ do
            cls [] `covers` [] `shouldBe` True
        it "is True for a clause that's a subset of the assignment" $ do
            cls [1, -2] `covers` [False, True, False] `shouldBe` True
        it "is True for a clause that's equal to the inverted assignment" $ do
            cls [-1, 2, 3, -4] `covers` [True, False, False, True] `shouldBe` True
        it "is False for a clause that's a superset of the assignment" $ do
            cls [-1, 2, 3, -4] `covers` [True, False, False] `shouldBe` False
        it "is False if the clause contains literals that aren't in the assignment" $ do
            cls [-2, 3, -4, 5] `covers` [True, True, False] `shouldBe` False

    describe "clauses" $ do
        it "is the empty clause for 0 variables" $ do
            clauses 0 `shouldBe` [cls []]
        it "is 3 clauses for 1 variable" $ do
            clauses 1 `shouldBe` map cls [[1], [-1], []]
        it "is 9 clauses for 2 variables" $ do
            clauses 2 `shouldBe` map cls [[1,2], [1,-2], [1], [-1,2], [-1,-2], [-1], [2], [-2], []]

