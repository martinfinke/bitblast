module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import TruthTable

spec :: Spec
spec = do
    describe "halfAdder" $ do
        it "creates an half adder circuit for two input and two output variables" $ do
            let [x,y,s,c] = map var [4,3,1,5]
            let ha = halfAdder (x,y) (s,c)

            ha `shouldBe` And [Equiv [Atom c, And [Atom x,Atom y]], Equiv [Atom s, Xor [Atom x,Atom y]]]