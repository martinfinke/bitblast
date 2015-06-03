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

-- Imperative port from Boolector's mul_aigvec. Has DontCare overflow.
multiplierSegmentDontCareOverflow :: [Formula] -> [Formula] -> [Formula]
multiplierSegmentDontCareOverflow xs ys =
    let res = [And [x, last ys] | x <- xs]
    in fst $ foldr outerLoop (res,false) [0..length xs - 2]
    where false = Or []
          len = length xs
          outerLoop i (res,cout) =
                let res' = fst $ foldr (innerLoop i) (res,cout) [0..i]
                in (res',false) 
          innerLoop i j (res,cout) =
                let andGate = And [xs!!(len - 1 - i + j), ys!!i]
                    tmp = res!!j
                    cin = cout
                    (adderSum,adderCout) = fullAdderSegment (tmp,andGate) cin
                in (replaceInList res j adderSum, adderCout)

replaceInList :: [a] -> Int -> a -> [a]
replaceInList ls i el =
    let (prefix,suffix) = splitAt i ls
    in prefix ++ [el] ++ tail suffix



-- See http://www.allsyllabus.com/aj/note/ECE/Digital_System_Design_Using_VHDL/Unit4/4-bit%20Multiplier%20Partial%20Products.php

firstRow :: [Formula] -> Formula -> [Formula]
firstRow xs y = [And [x,y] | x <- xs]

type Row = ([Formula], Formula)
-- Hier dürfen nur die n-1 sums rein, die von der firstRow tatsächlich weitergegeben werden!
secondRow :: [Formula] -> Formula -> [Formula] -> Row
secondRow xs y sumIns = (sumOuts, leftmostCarryOut)
    where sumOuts = leftmostSum : middleSums ++ [rightmostSum]
          products = [And [x,y] | x <- xs]
          (leftmostSum,leftmostCarryOut) = halfAdderSegment (head products, middleCarryOut)
          (rightmostSum,rightmostCarryOut) = halfAdderSegment (last products, last sumIns)
          (middleSums,middleCarryOut) = fullAdderChain (tail $ init products) (init sumIns) rightmostCarryOut

nthRow :: [Formula] -> Formula -> [Formula] -> Row
nthRow xs y sumIns = (sumOuts, leftmostCarryOut)
    where sumOuts = otherSums ++ [rightmostSum]
          products = [And [x,y] | x <- xs]
          (rightmostSum,rightmostCarryOut) = halfAdderSegment (last products, last sumIns)
          (otherSums,leftmostCarryOut) = fullAdderChain (init products) (init sumIns) rightmostCarryOut

fullAdderChain :: [Formula] -> [Formula] -> Formula -> ([Formula], Formula)
fullAdderChain products sumIns initialCIn = (finalSums,finalCOut)
    where (finalSums,finalCOut) = foldr makeFullAdder ([],initialCIn) $ zip products sumIns
          makeFullAdder (p,sumIn) (sums,cIn) =
                let (sOut,cOut) = fullAdderSegment (p,sumIn) cIn
                in (sOut:sums, cOut)

multiplierSegment :: [Formula] -> [Formula] -> [Formula]
multiplierSegment xs ys
    | hasSecondRow =
        let ((lastRowSums, finalCarryOut), accumSums) = foldr (connectRows xs) (second, [last secondRowSums, last firstRowSums]) (drop 2 lsbAtFrontYs)
        in finalCarryOut : (init lastRowSums) ++ accumSums
    | otherwise = firstRowSums
    where lsbAtFrontYs = reverse ys
          firstRowSums = firstRow xs (head lsbAtFrontYs)
          hasSecondRow = length ys > 1
          second@(secondRowSums,_) = secondRow xs (head $ tail lsbAtFrontYs) (init firstRowSums)

connectRows :: [Formula] -> Formula -> (Row, [Formula]) -> (Row, [Formula])
connectRows xs y ((previousSums, previousCarry), sumsAccum) =
    let row@(currentSums, _) = nthRow xs y (previousCarry : init previousSums)
    in (row, last currentSums : sumsAccum)

multiplier :: OverflowMode -> [Formula] -> [Formula] -> [Formula] -> Formula
multiplier overflowMode xs ys sums
    | length xs /= length ys = error "The input bit vectors must have the same width."
    | otherwise = And $ forbidOverflow ++ sumEquivs
    where sums' = multiplierSegment xs ys
          overflowingLength = length sums' - length xs
          overflowing = take overflowingLength sums' -- TODO: Use splitAt
          (forbidOverflow,nonOverflowing) = case overflowMode of
                Forbid -> (map Not overflowing, drop overflowingLength sums')
                _ -> ([], sums')
          sumEquivs = map (\(s,s') -> Equiv [s, s']) $ zip nonOverflowing sums