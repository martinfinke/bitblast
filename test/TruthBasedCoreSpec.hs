module TruthBasedCoreSpec where

import SpecHelper
import TruthBasedCore

spec :: Spec
spec = do
    let cls = Clause . map Lit
    let cnf = CNF . map cls
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

    describe "assignments" $ do
        it "is the empty assignment for 0 variables" $ do
            assignments 0 `shouldBe` [[]]
        it "is 2 assignments for 1 variable" $ do
            assignments 1 `shouldBe` [[False], [True]]
        it "is 4 assignments for 2 variables" $ do
            assignments 2 `shouldBe` [[False,False], [False,True], [True,False], [True,True]]

    describe "makeCnf" $ do
        let f [x,y,z] = x == (y && z)
        let g [x] = not x
        let h = const True
        it "finds a CNF for f with 1 extra variable and 3 allowed clauses" $ do
            makeCnf 3 f 1 3 `shouldReturn` Just (cnf [[1,-2,-3], [-1,2], [-1,3]])
        it "doesn't find a CNF for f with 1 extra variable and 2 allowed clauses" $ do
            makeCnf 3 f 1 2 `shouldReturn` Nothing
        it "finds a CNF for g with 1 extra variable and 1 allowed clause" $ do
            makeCnf 1 g 1 1 `shouldReturn` Just (cnf [[-1]])
        it "finds an empty CNF for h" $ do
            makeCnf 0 h 1 0 `shouldReturn` Just (cnf [])