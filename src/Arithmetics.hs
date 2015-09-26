module Arithmetics where

import Formula
import NormalForm
import Data.List(zip4)
import Debug.Trace(traceShow)
import Data.List(zip3)
import Text.Printf(printf)
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Variable
import Assignment
import TruthTable
import Tseitin(remapVars,replaceVars)
import Data.List(sort)
import Debug.Trace(traceShow)
import Data.Maybe

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

summerSegment :: Maybe Formula -> [Formula] -> [Formula] -> ([Formula], Formula)
summerSegment maybeCarry (x:[]) (y:[]) = ([s], cOut)
    where (s,cOut) = case maybeCarry of
            Nothing -> halfAdderSegment (x,y)
            Just carry -> fullAdderSegment (x,y) carry
summerSegment maybeCarry (x:xs) (y:ys)
    | length xs /= length ys = error "The input bit vectors must have the same width."
    | otherwise = (s:sums, cOut)
    where (s,cOut) = fullAdderSegment (x,y) finalC
          (sums,finalC) = summerSegment maybeCarry xs ys

data OverflowMode = Forbid | DontCare | Connect Formula | ToSum
    deriving(Eq, Show)

summer :: OverflowMode -> [Formula] -> [Formula] -> [Formula] -> Formula
summer = summerWithCarry Nothing
summerWithCarry :: Maybe Formula -> OverflowMode -> [Formula] -> [Formula] -> [Formula] -> Formula
summerWithCarry maybeCarry overflowMode xs ys sums
    | length xs /= length ys || length ys /= length sums = error "The input bit vectors must have the same width."
    | otherwise = And outputFormula
    where (sums', cOut') = summerSegment maybeCarry xs ys
          sumEquivs = map (\(s,s') -> Equiv [s, s']) $ zip sums' sums
          outputFormula = case overflowMode of
                Forbid -> Not cOut' : sumEquivs
                DontCare -> sumEquivs
                Connect cOut -> Equiv [cOut,cOut'] : sumEquivs

multiplierSegment :: [Formula] -> [Formula] -> [Formula]
multiplierSegment xs ys =
    let andGates y = [And [x, y] | x <- xs]
        firstRow = andGates (last ys)
        firstRowCarry = Or [] -- aka False
        (finalRow,finalCarry) = foldr (makeRow xs) (firstRow, firstRowCarry) (init ys)
    in finalCarry : finalRow

makeRow :: [Formula] -> Formula -> ([Formula], Formula) -> ([Formula], Formula)
makeRow xs y (previousRow,previousCOut) =
    let inputsFromPreviousRow = previousCOut : init previousRow
        numBits = length xs
        andGates = [And [x, y] | x <- xs]
        zipped = zip andGates inputsFromPreviousRow
        keepFromAbove = drop (numBits-1) previousRow
        (row,maybeCarry) = foldr makeAndConnectAdder (keepFromAbove, Nothing) zipped
    in (row,fromJust maybeCarry)

makeAndConnectAdder :: (Formula, Formula) -> ([Formula], Maybe Formula) -> ([Formula], Maybe Formula)
makeAndConnectAdder (andGate,inputFromPreviousRow) (accum,maybeCarryFromRight) =
    let (adderSum,adderCout) = case maybeCarryFromRight of
            Nothing -> halfAdderSegment (andGate, inputFromPreviousRow)
            Just carryFromRight -> fullAdderSegment (andGate, inputFromPreviousRow) carryFromRight
    in (adderSum:accum, Just adderCout)

nBitAddition :: OverflowMode -> Int -> Formula
nBitAddition overflowMode numBits =
    let (first,second,sums) = makeAndOrderAtoms numBits 1
    in summer overflowMode first second sums

-- | The n-th carry term for an adder with numBits bits. Useful for specifically replacing certain carries by extra variables.
adderCarry :: Int -> Int -> Formula
adderCarry 0 numBits = And [Atom $ var 0, Atom $ var numBits]
adderCarry which numBits =
    let current = (Atom . var $ which+numBits, Atom . var $ which)
        previous = adderCarry (which-1) numBits
    in Or [And [fst current, previous], And [snd current, previous], And [snd current, fst current]]

nBitMultiplication :: OverflowMode -> Int -> Formula
nBitMultiplication mode numBits
    | mode == DontCare = simplify $ And equivs
    | mode == Forbid = simplify $ And $ forbidOverflow ++ equivs
    | mode == ToSum = simplify $ And $ longEquivs
    | otherwise = error $ "nBitMultiplication: OverflowMode not implemented: " ++ show mode
    where vars = makeVars (4*numBits)
          atoms = makeAndOrderAtoms numBits $ if mode == ToSum then 2 else 1
          (first,second,sums) = atoms
          (overflowing,allowed) = splitAt numBits $ multiplierSegment first second
          forbidOverflow = map Not overflowing
          equivs = [Equiv [s,s'] | (s,s') <- zip sums allowed]
          longEquivs = [Equiv [s,s'] | (s,s') <- zip sums (overflowing++allowed)]


makeAndOrderAtoms :: Int -> Int -> ([Formula], [Formula], [Formula])
makeAndOrderAtoms numBits sumLengthFactor =
    let (first,second,sums) = makeAndOrderVars numBits sumLengthFactor
    in (map Atom first, map Atom second, map Atom sums)

-- | Variable ordering is: [x2,x1,x0] + [x5,x4,x3] = [x8,x7,x6].
makeAndOrderVars :: Int -> Int -> ([Variable], [Variable], [Variable])
makeAndOrderVars numBits sumLengthFactor = (first, second, sums)
    where vars = makeVars $ (2+sumLengthFactor)*numBits
          first = reverse $ take numBits vars
          second = reverse $ take numBits $ drop numBits vars
          sums = reverse $ take (sumLengthFactor*numBits) $ drop (2*numBits) vars

-- | The "all higher bits are equal" constraints are arranged so that they share common subtrees.
greaterThan, greaterThanEq :: Int -> Formula
greaterThan numBits =
    let (first,second,_) = makeAndOrderAtoms numBits 0
    in greater first second

greaterThanEq numBits =
    let (first,second,_) = makeAndOrderAtoms numBits 0
    in Not $ greater second first

greater :: [Formula] -> [Formula] -> Formula
greater first second =
    let zipped = zip (reverse first) (reverse second)
        ensureOneBit a b = And [a, Not b]
    in Or . fst $ foldr (\(a,b) (accum,allHigherEqual) -> (And (ensureOneBit a b : allHigherEqual) : accum, Equiv [a,b] : allHigherEqual)) ([],[]) zipped

additionTableBased, multiplicationTableBased :: OverflowMode -> Int -> Canonical
additionTableBased = operation3 (+)
multiplicationTableBased = operation3 (*)

lessThanTableBased, lessThanEqTableBased, greaterThanTableBased, greaterThanEqTableBased :: Int -> Canonical
lessThanTableBased = operation2 (<)
lessThanEqTableBased = operation2 (<=)
greaterThanTableBased = operation2 (>)
greaterThanEqTableBased = operation2 (>=)

-- | Encodes a binary operation (like "a < b") as a canonical CNF
operation2 :: (Int -> Int -> Bool) -> Int -> Canonical
operation2 op numBits = makeCanonical False numBits $ \bits x y -> do
        guard (x `op` y)
        return $ bits x ++ bits y

-- | Encodes a ternary operation (like "a+b=c" or "a*b=c") as a canonical CNF
operation3 :: (Int -> Int -> Int) -> OverflowMode -> Int -> Canonical
operation3 _ (Connect _) _ = error "Arithmetics.operation3: Connect overflow not available"
operation3 op overflowMode numBits = makeCanonical True numBits $ \bits x y -> do
        when (overflowMode == Forbid) $ guard ((x `op` y) < 2^numBits)
        return $ bits x ++ bits y ++ bits (x `op` y)

type RowMaker = (Int -> String) -> Int -> Int -> [String]

makeCanonical :: Bool -> Int -> RowMaker -> Canonical
makeCanonical hasResultBits numBits f =
    let (first,second,sums) = makeAndOrderVars numBits $ if hasResultBits then 1 else 0
        orderedVars = first ++ second ++ sums
        varSet = Set.fromList orderedVars
        trim str = drop (length str - numBits) str
        bits = trim . (printf $ "%0" ++ show numBits ++ "b") :: (Int -> String)
        range = [0..(2^numBits)-1]
        trueRowStrings = do
            x <- range
            y <- range
            f bits x y
        convertRowString str = assignmentFromList $ zip orderedVars $ map (== '1') str
        trues = map convertRowString trueRowStrings
        table = foldr (flip setRow True) (allFalseTable varSet) trues 
    in tableToCnf varSet table
