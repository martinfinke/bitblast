module Arithmetics where

import Formula
import Data.List(zip4)
import Debug.Trace(traceShow)
import Data.List(zip3)
import Text.Printf(printf)

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

-- Port from Boolector's mul_aigvec. Has DontCare overflow.
-- As opposed to Boolector's version, this one uses a half adder for the first (rightmost) element in each row. Boolector uses a full adder with an always-false carry input.
multiplierSegmentDontCareOverflow :: [Formula] -> [Formula] -> [Formula]
multiplierSegmentDontCareOverflow xs ys =
    let firstRow = [And [x, last ys] | x <- xs]
        forAllRemainingRows (i,y) previousRow =
              let relevantXs = drop (length xs - 1 - i) xs
                  andGates = [And [x,y] | x <- relevantXs]
                  (currentRow,_) = foldr innerLoop (drop (i+1) previousRow, Nothing) $ zip andGates previousRow
              in currentRow
        innerLoop (andGate,incomingSum) (res,maybeCarry) =
              let (adderSum,adderCout) = case maybeCarry of
                    Nothing -> halfAdderSegment (andGate,incomingSum)
                    Just carry -> fullAdderSegment (andGate,incomingSum) carry
              in (adderSum : res, Just adderCout)
    in foldr forAllRemainingRows firstRow $ zip [0..length ys - 2] (init ys)

-- | Doesn't care about overflow.
multiplier :: [Formula] -> [Formula] -> [Formula] -> Formula
multiplier xs ys sums
    | length xs /= length ys = error "The factors must have the same width."
    | otherwise = And outputFormula
    where sums' = multiplierSegmentDontCareOverflow xs ys
          outputFormula = map (\(s,s') -> Equiv [s, s']) $ zip sums' sums