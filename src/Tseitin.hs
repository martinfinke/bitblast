module Tseitin where

import Variable
import Formula
import qualified Data.Set as Set

tseitinReplace :: Set.Set Variable -> Formula -> Formula -> Maybe (Formula,Variable)
tseitinReplace varSet toReplace formula =
    let extraVar = succ $ Set.findMax varSet
    in case findAndReplace toReplace extraVar formula of
            (False,_) -> Nothing
            (True,newFormula) -> Just (And [Equiv [Atom extraVar, toReplace], newFormula], extraVar)

findAndReplace :: Formula -> Variable -> Formula -> (Bool,Formula)
findAndReplace toReplace extraVar formula
    | toReplace == formula = (True, Atom extraVar)
    | otherwise = case formula of
            Atom _ -> (False, formula)
            Not f -> fmap Not $ findAndReplace toReplace extraVar f
            And fs -> recurseList And fs
            Or fs -> recurseList Or fs
            Implies p c -> recurseImplies p c
            Xor fs -> recurseList Xor fs
            Equiv fs -> recurseList Equiv fs
    where recurse = findAndReplace toReplace extraVar
          recurseList op fs =
            let fs' = map recurse fs
            in if any fst fs'
                then (True, op $ map snd fs')
                else (False, op fs)
          recurseImplies p c =
            let [p',c'] = map recurse [p,c]
            in if fst p' || fst c'
                then (True, Implies (snd p') (snd c'))
                else (False, Implies p c)
