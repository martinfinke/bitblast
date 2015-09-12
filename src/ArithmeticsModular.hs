module ArithmeticsModular where

import Arithmetics
import Formula
import Variable(makeVars)
import Tseitin(remapVars,replaceVars)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List(sort)

combine :: (Int,Int) -> Formula -> Formula -> Formula
combine (lBits,hBits) low oldHigh =
    let high = remapVars (variableSet low) oldHigh
        [lVars,hVars] = map (Set.toAscList . variableSet) [low,high]
        (lFst,lSnd,lRes,lCIn,lCout,lExtra) = bitVectors lBits lVars
        (hFst,hSnd,hRes,hCIn,hCout,hExtra) = bitVectors hBits hVars
        connectCarry = Equiv [Atom lCout, Atom hCIn]
        joined = And [low, high, connectCarry]
        joinedExtraVars = lCout : hCIn : lExtra ++ hExtra
        order = lFst ++ hFst ++ lSnd ++ hSnd ++ lRes ++ hRes ++ [lCIn, hCout] ++ joinedExtraVars
        mapping = Map.fromList $ zip order (sort $ lVars ++ hVars)
    in replaceVars mapping joined

bitVectors :: Int -> [a] -> ([a], [a], [a], a, a, [a])
bitVectors numBits vars
    | length vars < 3*numBits + 2 = error $ "bitVectors: variable list too short: " ++ show (length vars) ++ " < " ++ show (3*numBits + 2)
    | otherwise = (take numBits vars, take numBits $ drop numBits vars, take numBits $ drop (2*numBits) vars, vars!!(3*numBits), vars!!(3*numBits+1), drop (3*numBits+2) vars)

summerModule :: Int -> Formula
summerModule numBits =
    let (first,second,result,cIn,cOut,_) = bitVectors numBits . map Atom $ makeVars (3*numBits+2)
    in summerWithCarry (Just cIn) (Connect cOut) (reverse first) (reverse second) (reverse result)

forbidOverflow :: Int -> Formula -> Formula
forbidOverflow numBits f =
    let vars = Set.toAscList (variableSet f)
        (_,_,_,_,cOut,_) = bitVectors numBits vars
    in And [f, Not $ Atom cOut]

noCarryIn :: Int -> Formula -> Formula
noCarryIn numBits f =
    let vars = Set.toAscList (variableSet f)
        (_,_,_,cIn,_,_) = bitVectors numBits vars
    in And [f, Not $ Atom cIn]








