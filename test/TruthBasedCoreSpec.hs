module TruthBasedCoreSpec where

import SpecHelper
import TruthBasedCore


cls = Clause . map Lit

cnf = CNF . map cls


spec :: Spec
spec = do
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


    let f [x,y,z] = x == (y && z)
    let g [x] = not x
    let h = const True
    describe "makeCnf" $ do
        it "finds a CNF for f with 1 extra variable and 7 allowed literals" $ do
            makeCnf 3 f 1 (clauses 4) 7 `shouldReturn` Just (cnf [[1,-2,-3], [-1,2], [-1,3]])
        it "doesn't find a CNF for f with 1 extra variable and 2 allowed literals" $ do
            makeCnf 3 f 1 (clauses 4) 2 `shouldReturn` Nothing
        it "finds a CNF for g with 1 extra variable and 1 allowed literal" $ do
            makeCnf 1 g 1 (clauses 2) 1 `shouldReturn` Just (cnf [[-1]])
        it "finds an empty CNF for h" $ do
            makeCnf 0 h 1 (clauses 1) 0 `shouldReturn` Just (cnf [])

    describe "table" $ do
        let (x:y:_) = variableNumbers
        describe "for 0 variables and 0 extra vars" $ do
            it "returns an empty table" $ do
                table 0 (const True) 0 `shouldBe` Table [[]] [Clause []] [(True, [True])]
                table 0 (const False) 0 `shouldBe` Table [[]] [Clause []] [(False, [True])]
        describe "for 1 variable and 0 extra vars" $ do
            let f (x:[]) = not x
            it "creates the correct table" $ do
                let expectedClauses = [Clause [lit x True], Clause [lit x False], Clause []]
                let expectedRows = [
                        (True, [True, False, True]),
                        (False, [False, True, True])
                        ]
                table 1 f 0 `shouldBe` Table [[False], [True]] expectedClauses expectedRows
        describe "for 0 variables and 1 extra var" $ do
            it "creates the correct table" $ do
                let f = const True
                let expectedClauses = [Clause [lit x True], Clause [lit x False], Clause []]
                table 0 f 1 `shouldBe` Table [[False], [True]] expectedClauses [(True, [True, False, True]), (True, [False, True, True])]

        describe "for 1 variables and 1 extra var" $ do
            let f (x:[]) = x
            it "does" $ do
                let expectedClauses = [
                        Clause [lit x True, lit y True],
                        Clause [lit x True, lit y False],
                        Clause [lit x True],
                        Clause [lit x False, lit y True],
                        Clause [lit x False, lit y False],
                        Clause [lit x False],
                        Clause [lit y True],
                        Clause [lit y False],
                        Clause []
                        ]
                let expectedAssignments = [[False, False], [False, True], [True, False], [True, True]]
                let expectedRows = [
                        (False, [True, False, True, False, False, False, True, False, True]),
                        (False, [False, True, True, False, False, False, False, True, True]),
                        (True, [False, False, False, True, False, True, True, False, True]),
                        (True, [False, False, False, False, True, True, False, True, True])
                        ]
                table 1 f 1 `shouldBe` Table expectedAssignments expectedClauses expectedRows