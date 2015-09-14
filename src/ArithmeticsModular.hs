module ArithmeticsModular where

import Arithmetics
import Formula
import Variable
import Tseitin(remapVars,replaceVars)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List(sort)
import Data.List.Split(chunksOf)

combineAdd :: (Int,Int) -> Formula -> Formula -> Formula
combineAdd (lBits,hBits) low oldHigh =
    let high = remapVars (variableSet low) oldHigh
        [lVars,hVars] = map (Set.toAscList . variableSet) [low,high]
        (lFst,lSnd,lRes,lCIn,lCout,lExtra) = addBitVectors lBits lVars
        (hFst,hSnd,hRes,hCIn,hCout,hExtra) = addBitVectors hBits hVars
        connectCarry = Equiv [Atom lCout, Atom hCIn]
        joined = And [low, high, connectCarry]
        joinedExtraVars = lCout : hCIn : lExtra ++ hExtra
        order = lFst ++ hFst ++ lSnd ++ hSnd ++ lRes ++ hRes ++ [lCIn, hCout] ++ joinedExtraVars
        mapping = Map.fromList $ zip order (sort $ lVars ++ hVars)
    in replaceVars mapping joined

addBitVectors :: Int -> [a] -> ([a], [a], [a], a, a, [a])
addBitVectors numBits vars
    | length vars < 3*numBits + 2 = error $ "addBitVectors: variable list too short: " ++ show (length vars) ++ " < " ++ show (3*numBits + 2)
    | otherwise = (take numBits vars, take numBits $ drop numBits vars, take numBits $ drop (2*numBits) vars, vars!!(3*numBits), vars!!(3*numBits+1), drop (3*numBits+2) vars)

summerModule :: Int -> Formula
summerModule numBits =
    let (first,second,result,cIn,cOut,_) = addBitVectors numBits . map Atom $ makeVars (3*numBits+2)
    in summerWithCarry (Just cIn) (Connect cOut) (reverse first) (reverse second) (reverse result)

forbidOverflow :: Int -> Formula -> Formula
forbidOverflow numBits f =
    let vars = Set.toAscList (variableSet f)
        (_,_,_,_,cOut,_) = addBitVectors numBits vars
    in And [f, Not $ Atom cOut]

noCarryIn :: Int -> Formula -> Formula
noCarryIn numBits f =
    let vars = Set.toAscList (variableSet f)
        (_,_,_,cIn,_,_) = addBitVectors numBits vars
    in And [f, Not $ Atom cIn]

-- | finalNumBits is the length of an operand of the final circuit. The output of the final circuit has the length: 2*finalNumBits.
-- NOT WORKING CORRECTLY YET
combineMul :: Int -> Formula -> Formula
combineMul finalNumBits halfWidthMul =
    let numExtraVars = 4*finalNumBits + 4*numExtraPerQuarter + 1 + finalNumBits
        totalNumVars = 4*finalNumBits + numExtraVars
        (lhs,rhs,finalResult,extra) = mulBitVectors finalNumBits $ makeVars totalNumVars
        (lLhs,hLhs):(lRhs,hRhs):_ = map (splitAt $ finalNumBits `div` 2) [lhs,rhs]
        quarterVars = Set.toAscList $ variableSet halfWidthMul
        numVarsPerQuarter = length quarterVars
        numExtraPerQuarter = numVarsPerQuarter - 2*finalNumBits
        topLeftResult:topRightResult:bottomLeftResult:bottomRightResult:_ = chunksOf finalNumBits extra
        extra' = drop (4*finalNumBits) extra
        topLeftExtra:topRightExtra:bottomLeftExtra:bottomRightExtra:_ = chunksOf numExtraPerQuarter extra'
        extra'' = drop (4*numExtraPerQuarter) extra'
        makeQuarter varList = replaceVars (Map.fromList $ zip (Set.toAscList (variableSet halfWidthMul)) varList) halfWidthMul
        topLeft = makeQuarter $ hLhs ++ lRhs ++ topLeftResult ++ topLeftExtra
        topRight = makeQuarter $ lLhs ++ lRhs ++ topRightResult ++ topRightExtra
        bottomLeft = makeQuarter $ hLhs ++ hRhs ++ bottomLeftResult ++ bottomLeftExtra
        bottomRight = makeQuarter $ lLhs ++ hRhs ++ bottomRightResult ++ bottomRightExtra
        intermediateCOut = Atom $ head extra''
        intermediateResult = map Atom $ take finalNumBits $ drop 1 extra''

        intermediateSum = summer (Connect intermediateCOut) (map Atom topLeftResult) (map Atom bottomRightResult) intermediateResult
        false = Or []
        intermediateResultDoubleWidth = replicate ((finalNumBits `div` 2) - 1) false ++ [intermediateCOut] ++ intermediateResult ++ replicate (finalNumBits `div` 2) false
        finalSum = summer DontCare intermediateResultDoubleWidth (map Atom $ bottomLeftResult ++ topRightResult) (map Atom finalResult)
    in And [topLeft,topRight,bottomLeft,bottomRight,intermediateSum,finalSum]

mulBitVectors :: Int -> [a] -> ([a], [a], [a], [a])
mulBitVectors numBits vars
    | length vars < 4*numBits = error $ "mulBitVectors: variable list too short: " ++ show (length vars) ++ " < " ++ show (4*numBits)
    | otherwise = (take numBits vars, take numBits $ drop numBits vars, take (2*numBits) $ drop (2*numBits) vars, drop (4*numBits) vars)

multiplierModule :: Int -> Formula
multiplierModule numBits = nBitMultiplication ToSum numBits
