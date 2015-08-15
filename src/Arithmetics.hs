module Arithmetics where

import Formula
import NormalForm
import Data.List(zip4)
import Debug.Trace(traceShow)
import Data.List(zip3)
import Text.Printf(printf)
import Control.Monad
import qualified Data.Set as Set
import Variable

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
    deriving(Eq)

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

-- | Has DontCare overflow.
multiplier :: [Formula] -> [Formula] -> [Formula] -> Formula
multiplier xs ys sums
    | length xs /= length ys = error "The factors must have the same width."
    | otherwise = And outputFormula
    where sums' = multiplierSegmentDontCareOverflow xs ys
          outputFormula = map (\(s,s') -> Equiv [s, s']) $ zip sums' sums

nBitAddition :: OverflowMode -> Int -> (Formula, Set.Set Variable)
nBitAddition overflowMode numBits =
    -- Variable ordering is [x2,x1,x0] + [x5,x4,x3] = [x8,x7,x6]
    let vars = makeVars (3*numBits)
        atoms = map Atom vars
        first = reverse $ take numBits atoms
        second = reverse $ take numBits $ drop numBits atoms
        sums = reverse $ take numBits $ drop (2*numBits) atoms
        summerCircuit = summer overflowMode first second sums
    in (summerCircuit,Set.fromList vars)

nBitMultiplication :: OverflowMode -> Int -> (Formula, Set.Set Variable)
nBitMultiplication Forbid numBits =
    let (canonical,varSet) = multiplication Forbid numBits
    in (getFormula canonical, varSet)
nBitMultiplication DontCare numBits = 
    let vars = makeVars (3*numBits)
        atoms = map Atom vars
        first = reverse $ take numBits atoms
        second = reverse $ take numBits $ drop numBits atoms
        sums = reverse $ take numBits $ drop (2*numBits) atoms
        multiplierCircuit = multiplier first second sums
    in (multiplierCircuit,Set.fromList vars)
nBitMultiplication _ _ = error "nBitMultiplication: OverflowMode not implemented."

-- has DontCare overflow
multiplicationTableGen :: Int -> Int -> [String]
multiplicationTableGen termBits resultBits =
    let formatString = "%0" ++ show termBits ++ "b"
        resultFormatString = "%0" ++ show resultBits ++ "b"
        range = [0..(2^termBits)-1] :: [Int]
        calculate = (*)
        printResult i j =
            let str = printf resultFormatString (calculate i j) :: String
            in drop (length str - resultBits) str
        rows = [printf formatString i ++ printf formatString j ++ printResult i j | i <- range, j <- range] :: [String]
    in rows

multiplication :: OverflowMode -> Int -> (Canonical, Set.Set Variable)
multiplication = operation3 (*)

lessThan :: Int -> (Canonical, Set.Set Variable)
lessThan = operation2 (<)

lessThanEq :: Int -> (Canonical, Set.Set Variable)
lessThanEq = operation2 (<=)

operation2 :: (Int -> Int -> Bool) -> Int -> (Canonical, Set.Set Variable)
operation2 op numBits =
    let vars = makeVars (2*numBits)
        varSet = Set.fromList vars
        first = reverse $ take numBits vars
        second = reverse $ take numBits $ drop numBits vars
        orderedVars = first ++ second
        trim str = drop (length str - numBits) str
        bits = trim . (printf $ "%0" ++ show numBits ++ "b") :: (Int -> String)
        range = [0..(2^numBits)-1]
        trueRowStrings = do
            x <- range
            y <- range
            guard (x `op` y)
            return $ bits x ++ bits y
        convertRowString str = assignmentFromList $ zip orderedVars $ map (== '1') str
        trues = map convertRowString trueRowStrings
        table = foldr (flip setRow True) (allFalseTable varSet) trues 
    in (tableToCnf varSet table, varSet)

-- | Encodes an operation with 3 numbers (like "a+b=c" or "a*b=c") as a canonical CNF
operation3 :: (Int -> Int -> Int) -> OverflowMode -> Int -> (Canonical, Set.Set Variable)
operation3 op overflowMode numBits =
    let vars = makeVars (3*numBits)
        varSet = Set.fromList vars
        first = reverse $ take numBits vars
        second = reverse $ take numBits $ drop numBits vars
        sums = reverse $ take numBits $ drop (2*numBits) vars
        orderedVars = first ++ second ++ sums
        trim str = drop (length str - numBits) str
        bits = trim . (printf $ "%0" ++ show numBits ++ "b") :: (Int -> String)
        range = [0..(2^numBits)-1]
        trueRowStrings = do
            x <- range
            y <- range
            when (overflowMode == Forbid) $ guard (x `op` y < 2^numBits)
            return $ bits x ++ bits y ++ bits (x `op` y)
        convertRowString str = assignmentFromList $ zip orderedVars $ map (== '1') str
        trues = map convertRowString trueRowStrings
        table = foldr (flip setRow True) (allFalseTable varSet) trues 
    in (tableToCnf varSet table, varSet)
