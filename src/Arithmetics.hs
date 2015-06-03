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

-- Imperative port from Boolector's mul_aigvec. Has DontCate overflow.
multiplierSegment' :: [Formula] -> [Formula] -> [Formula]
multiplierSegment' xs ys = fst $ flip State.execState ([And [x, last ys] | x <- xs], false) $ do
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







-- Non-functional combinatorial multiplier below:

topRow :: [Formula] -> Formula -> ([Formula], [Formula])
topRow xs y = (sumOuts, cOuts)
    where blocks = [halfAdderSegment (And [x,y], Or []) | x <- xs] -- Actually there's no need for a half adder here, the AND is enough
          (sumOuts,cOuts) = (map fst blocks, map snd blocks)

row :: [Formula] -> [Formula] -> [Formula] -> Formula -> ([Formula],[Formula])
row (x:xs) sumIns (cIn:cIns) y
    | length xs /= length sumIns || length xs /= length cIns = error $ printf "Inputs have wrong lengths: %d %d %d" (length $ x:xs) (length sumIns) (length $ cIn:cIns)
    | otherwise = (sumOuts, cOuts)
    where firstBlock = halfAdderSegment (And [x,y], traceShow ("    cIn: " ++ show cIn )$ cIn)
          blocks = firstBlock : [fullAdderSegment (sumIn,And [x',y]) cIn' | (sumIn,x',cIn') <- zip3 sumIns xs cIns]
          (sumOuts,cOuts) = (map fst blocks, map snd blocks)

-- | The xs and ys should have the LSB at the end. The sum outputs will have the LSB at the end.
multiplierSegment :: [Formula] -> [Formula] -> [Formula]
multiplierSegment xs ys = 
    let lsbAtFrontYs = reverse ys
        firstRow = topRow xs (head lsbAtFrontYs)
        (rows,sums) = foldr (connectRows xs) (firstRow,[]) (tail lsbAtFrontYs)
        (lastRowSums,(msbCarry:_)) = rows
    in msbCarry : lastRowSums ++ sums

connectRows :: [Formula] -> Formula -> (([Formula],[Formula]),[Formula]) -> (([Formula],[Formula]),[Formula])
connectRows xs y ((lastSums,lastCs),sums) =
    let currentRow = row xs (init lastSums) lastCs y
    in (currentRow,last lastSums : sums)