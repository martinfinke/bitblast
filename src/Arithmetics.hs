module Arithmetics where

import Formula
import TruthTable(Variable)

halfAdder :: (Formula, Formula) -> (Formula, Formula) -> Formula
halfAdder (x,y) (s,c) = And [assertC, assertS]
    where assertC = Equiv [c, And [x,y]]
          assertS = Equiv [s, Xor [x,y]]

fullAdder :: (Formula, Formula) -> (Formula, Formula) -> Formula -> Formula
fullAdder (x,y) (cIn,cOut) s = And [assertCOut, assertS]
    where assertCOut = Equiv [cOut, Or [And [y, cIn], And [x, cIn], And [x, y]]]
          assertS = Equiv [s, Xor [x, y, cIn]]
