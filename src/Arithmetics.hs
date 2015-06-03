module Arithmetics where

import Formula
import Data.List(zip4)
import Debug.Trace(traceShow)
import Data.List(zip3)
import Text.Printf(printf)
import qualified Control.Monad.Trans.State.Lazy as State
import Control.Monad(forM_)

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
multiplierSegmentDontCareOverflow xs ys = fst $ flip State.execState ([And [x, last ys] | x <- xs], false) $ do
    let len = length xs
    let outerLoopRange = reverse [0..length xs - 2]
    forM_ outerLoopRange $ \i -> do
        State.modify $ storeCout false
        let innerLoopRange = reverse [0..i]
        forM_ innerLoopRange $ \j -> do
            let andGate = And [xs!!(len - 1 - i + j), ys!!i]
            (res,cout) <- State.get
            let tmp = res!!j
            let cin = cout
            let (adderSum,adderCout) = fullAdderSegment (tmp,andGate) cin
            State.modify $ storeCout adderCout
            State.modify $ storeRes (replaceInList res j adderSum)
    where false = Or []

storeRes newRes (_,oldCout) = (newRes,oldCout)
storeCout newCout (oldRes,_) = (oldRes,newCout)

replaceInList :: [a] -> Int -> a -> [a]
replaceInList ls i el =
    let (prefix,suffix) = splitAt i ls
    in prefix ++ [el] ++ tail suffix



-- See http://www.allsyllabus.com/aj/note/ECE/Digital_System_Design_Using_VHDL/Unit4/4-bit%20Multiplier%20Partial%20Products.php
-- oberste Reihe sind nur AND aus x_i und y_0
-- in der zweiten Reihe sind der erste und letzte ein Halb-Adder, und alles in der Mitte ein fullAdder
-- in allen folgenden Reihen ist der letzte ein Halb-Adder, alles andere sind Full-Adder

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
          hasSecondRow = length lsbAtFrontYs > 1
          second@(secondRowSums,_) = secondRow xs (head $ tail lsbAtFrontYs) (init firstRowSums)

connectRows :: [Formula] -> Formula -> (Row, [Formula]) -> (Row, [Formula])
connectRows xs y ((previousSums, previousCarry), sumsAccum) =
    let row@(currentSums, _) = nthRow xs y (previousCarry : init previousSums)
    in (row, last currentSums : sumsAccum)

