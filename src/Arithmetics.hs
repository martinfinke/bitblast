module Arithmetics where

import Formula
import TruthTable(Variable)

halfAdder :: (Variable, Variable) -> (Variable, Variable) -> Formula
halfAdder (x,y) (s,c) = And [Equiv [Atom c, And [Atom x,Atom y]], Equiv [Atom s, Xor [Atom x,Atom y]]]