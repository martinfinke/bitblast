module CNFSpec where

import Test.Hspec
import Test.QuickCheck
import CNF

cnfWithOnlyEmptyClauses :: CNF
cnfWithOnlyEmptyClauses = (And EmptyFalse (And EmptyFalse (And EmptyFalse EmptyTrue)))

instance Arbitrary Literal where
    arbitrary = do
        randomVarName <- fmap show $ choose ('a', 'z') :: Gen String
        posOrNeg <- elements [Pos, Neg]
        return $ posOrNeg randomVarName

instance Arbitrary Clause where
    arbitrary = do
        numLiterals <- choose (20,100) :: Gen Int
        randomLiteralList <- vectorOf numLiterals arbitrary :: Gen [Literal]
        return $ clauseFromList randomLiteralList

spec :: Spec
spec = do
    describe "Literal Show instance" $ do
        it "can show a positive literal" $
            show (Pos "foo") `shouldBe` "foo"
        it "can show a negative literal" $
            show (Neg "bar") `shouldBe` "-bar"

    describe "Clause Show instance" $ do
        it "can show an empty clause" $
            show (EmptyFalse) `shouldBe` ""
        it "can show a clause with one positive literal" $
            show (Or (Pos "x") EmptyFalse) `shouldBe` "x"
        it "can show a clause with two literals" $
            show (Or (Neg "y") (Or (Pos "z") EmptyFalse)) `shouldBe` "-y | z"

    describe "CNF Show instance" $ do
        it "can show an empty CNF" $
            show (EmptyTrue) `shouldBe` ""
        it "can show a CNF with one clause" $
            show (And (Or (Neg "baz") EmptyFalse) EmptyTrue) `shouldBe` "(-baz)"
        it "can show a CNF with two clauses" $
            show (And (Or (Pos "foo") EmptyFalse) (And (Or (Neg "bar") EmptyFalse) EmptyTrue)) `shouldBe` "(foo) & (-bar)"

    describe "clauseToList" $ do
        it "creates the empty list from an empty clause" $
            clauseToList EmptyFalse `shouldBe` []
        it "creates a list with one literal from a clause with one literal" $
            property $ \literal -> clauseToList (Or literal EmptyFalse) `shouldBe` [literal]
        it "creates a list with two literals from a clause with two literals" $
            property $ \lit1 lit2 -> clauseToList (Or lit1 (Or lit2 EmptyFalse)) `shouldBe` [lit1, lit2]
        it "is inverse to clauseFromList" $
            property $ \list -> (clauseToList . clauseFromList) list == list

    describe "cnfFromList" $ do
        it "doesn't add empty clauses" $ (case cnfFromList [[]] of
                EmptyTrue -> True
                _ -> False) `shouldBe` True

    describe "cnfToList" $ do
        it "creates the empty list from an empty CNF" $
            cnfToList EmptyTrue `shouldBe` []
        it "creates the empty list from a pseudo-nonempty CNF" $
            cnfToList cnfWithOnlyEmptyClauses `shouldBe` []
        it "creates a list with one list of literals from a CNF with one clause" $
            property $ \clause -> cnfToList (And clause EmptyTrue) `shouldBe` [clauseToList clause]
        it "creates a list with two lists of literals from a clause with two literals" $
            property $ \clause1 clause2 -> cnfToList (And clause1 (And clause2 EmptyTrue)) `shouldBe` map clauseToList [clause1, clause2]
        it "is NOT inverse to cnfFromList if there are empty clauses" $
            (cnfToList . cnfFromList) [[]] `shouldBe` []

    describe "Clause Eq instance" $ do
        it "is equal for two empty clauses" $
            EmptyFalse `shouldBe` EmptyFalse
        it "is equal for two clauses with the same single literal" $
            Or (Pos "x") EmptyFalse `shouldBe` Or (Pos "x") EmptyFalse
        it "doesn't care about the order of literals" $
            Or (Pos "a") (Or (Pos "b") EmptyFalse) `shouldBe` Or (Pos "b") (Or (Pos "a") EmptyFalse)

    describe "Literal Ord instance" $ do
        it "doesn't care about positive/negative" $
            property $ \lit1 lit2 -> compare lit1 lit2 == compare (varFromLiteral lit1) (varFromLiteral lit2)