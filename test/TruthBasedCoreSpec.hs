module TruthBasedCoreSpec where

import SpecHelper
import TruthBasedCore
import TruthBased
import Formula
import qualified Variable as V


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

    describe "illegalClauses" $ do
        let [x1,x2,x3] = map Atom $ V.makeVars 3
        it "works for an empty formula that is always True" $ do
            illegalClauses 0 (toTable 1 $ And []) `shouldBe` [Clause []]
        it "works for an empty formula that is always False" $ do
            illegalClauses 0 (toTable 2 $ And [Or []]) `shouldBe` []
        it "works for a formula with a single clause containing a single literal" $ do
            illegalClauses 1 (toTable 3 x1) `shouldBe` [Clause [lit 1 False], Clause []]
        it "works for an And over two variables" $ do
            illegalClauses 2 (toTable 2 $ And [x1,x2]) `shouldBe` [Clause [lit 1 False, lit 2 False], Clause [lit 1 False], Clause [lit 2 False], Clause []]

    describe "attemps" $ do
        describe "when given no lowest" $ do
            it "behaves correctly if there's too many attemps for the number range" $ do
                attempts 4 (-1) 1 `shouldBe` [0]
                attempts 4 (-1) 2 `shouldBe` [1]
                attempts 4 (-1) 3 `shouldBe` [1,2]
                attempts 1 (-1) 0 `shouldBe` []
            it "always returns best-1 if num=1" $ do
                property $ \i -> if i > 0
                    then attempts 1 (-1) i `shouldBe` [i-1]
                    else attempts 1 (-1) i `shouldBe` []
            it "never returns negative values" $ do
                property $ \(OneHundredOrLess i) (OneHundredOrLess j) -> any (< 0) (attempts i (-1) j) `shouldBe` False
            it "works for a big even number" $ do
                attempts 2 (-1) 100 `shouldBe` [50,99]
                attempts 3 (-1) 100 `shouldBe` [50,74,99]
                attempts 4 (-1) 100 `shouldBe` [50,66,82,99]
            it "works for a big odd number" $ do
                attempts 1 (-1) 99 `shouldBe` [98]
                attempts 2 (-1) 99 `shouldBe` [49,98]
                attempts 3 (-1) 99 `shouldBe` [49,73,98]

        describe "when given a lowest" $ do
            it "doesn't use best/2 as a lower bound if the lowest is higher" $ do
                attempts 2 15 20 `shouldBe` [15, 19]
                attempts 2 19 20 `shouldBe` [19]
            it "doesn't duplicate values if lowest == best/2" $ do
                attempts 3 10 20 `shouldBe` [10, 14, 19]
                attempts 3 1 2 `shouldBe` [1]
            
            
