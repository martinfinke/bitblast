module Arithmetics where

import Formula
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

summerSegment :: [Formula] -> [Formula] -> ([Formula], Formula)
summerSegment (x:[]) (y:[]) = ([s], cOut)
    where (s,cOut) = halfAdderSegment (x,y)
summerSegment (x:xs) (y:ys)
    | length xs /= length ys = error "The input bit vectors must have the same width."
    | otherwise = (s:sums, cOut)
    where (s,cOut) = fullAdderSegment (x,y) finalC
          (sums,finalC) = summerSegment xs ys

data OverflowMode = Forbid | DontCare | Connect Formula

summer :: OverflowMode -> [Formula] -> [Formula] -> [Formula] -> Formula
summer overflowMode xs ys sums
    | length xs /= length ys || length ys /= length sums = error "The input bit vectors must have the same width."
    | otherwise = And outputFormula
    where (sums', cOut') = summerSegment xs ys
          sumEquivs = map (\(s,s') -> Equiv [s, s']) $ zip sums' sums
          outputFormula = case overflowMode of
                Forbid -> Not cOut' : sumEquivs
                DontCare -> sumEquivs
                Connect cOut -> Equiv [cOut,cOut'] : sumEquivs

multiplierSegment :: [Formula] -> [Formula] -> [Formula]
multiplierSegment (x:[]) (y:[]) = [And [x,y]]
multiplierSegment xs ys = undefined

partialProducts :: Int -> [Formula] -> [Formula] -> [Formula]
partialProducts 0 (x0:[]) (y0:[]) = [And [x0,y0]]
partialProducts 1 (x0:x1:[]) (y0:y1:[]) = partialProducts 0 [x0] [y1] ++ partialProducts 0 [x1] [y0]
partialProducts 2 (x0:x1:x2:[]) (y0:y1:y2:[]) = partialProducts 1 [x0,x2] [y0,y2] ++ partialProducts 0 [x1] [y1]

-- partialProducts(0) = [a0 && b0]
-- partialProducts(1) = [a0 && b1, a1 && b0]
-- partialProducts(2) = [a2 && b0, a1 && b1, a0 && b2]
--  = partialProducts(1) von den beiden Enden ++ partialProducts(0) von denen in der Mitte
-- partialProducts(3) = [a3 && b0, a2 && b1, a1 && b2, a0 && b3]
--  = partialProducts(1) von den beiden Enden ++ partialProducts(1) von denen in der Mitte

buildingBlock :: Formula -> (Formula,Formula) -> Formula -> (Formula,Formula)
buildingBlock sumIn (x,y) cIn = (sumOut,cOut)
    where (sumOut,cOut) = fullAdderSegment (sumIn,And [x,y]) cIn
