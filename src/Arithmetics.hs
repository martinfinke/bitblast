module Arithmetics where

import Formula
import TruthTable(Variable)
import Data.List(zip4)
import Debug.Trace(traceShow)

halfAdderSegment :: (Formula,Formula) -> (Formula,Formula)
halfAdderSegment (x,y) = (s,c)
    where s = Xor [x,y]
          c = And [x,y]

halfAdder :: (Formula, Formula) -> (Formula, Formula) -> Formula
halfAdder (x,y) (s,c) = And [Equiv [s, s'], Equiv [c, c']]
    where (s',c') = halfAdderSegment (x,y)

fullAdderSegment :: (Formula,Formula) -> Formula -> (Formula,Formula)
fullAdderSegment (x,y) cIn = (s,cOut)
    where s = Xor [x, y, cIn]
          cOut = Or [And [y, cIn], And [x, cIn], And [x, y]]

fullAdder :: (Formula,Formula) -> (Formula,Formula) -> Formula -> Formula
fullAdder (x,y) (cIn,cOut) s = And [Equiv [s, s'], Equiv [cOut, cOut']]
    where (s',cOut') = fullAdderSegment (x,y) cIn





--makeSum :: (Formula,Formula,Formula) -> Formula
--makeSum (x,y,c) = Xor [x, y, c]

--calcCs :: (Formula, Formula) -> (Formula, [Formula]) -> (Formula, [Formula])
--calcCs (x,y) (lastC, accum) =
--    let current = Or [And [y, lastC], And [x, lastC], And [x, y]]
--    in (current, current:accum)

--summer :: [Formula] -> [Formula] -> (Formula, Formula) -> [Formula] -> Formula
--summer xs ys (cIn,cOut) sums =
--    let (cOut', cs) = foldr calcCs (cIn, cIn:[]) (zip xs ys)
--        sums' = map makeSum (zip3 xs ys $ tail cs)
--        sumsAreEqual = zipWith (\s1 s2 -> Equiv [s1, s2]) sums sums'
--    in And $ Equiv [cOut, cOut'] : sumsAreEqual
