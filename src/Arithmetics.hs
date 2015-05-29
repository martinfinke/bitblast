module Arithmetics where

import Formula
import TruthTable(Variable)

halfAdder :: (Variable, Variable) -> (Variable, Variable) -> Formula
halfAdder (x,y) (s,c) = And [assertC, assertS]
    where assertC = Equiv [Atom c, And [Atom x,Atom y]]
          assertS = Equiv [Atom s, Xor [Atom x,Atom y]]

fullAdder :: (Variable, Variable) -> (Variable, Variable) -> Variable -> Formula
fullAdder (x,y) (cIn,cOut) s = And [assertCOut, assertS]
    where assertCOut = Equiv [Atom cOut, Or [And [Atom y, Atom cIn], And [Atom x, Atom cIn], And [Atom x, Atom y]]]
          assertS = Equiv [Atom s, Xor [Atom x, Atom y, Atom cIn]]