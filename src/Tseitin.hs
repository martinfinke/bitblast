module Tseitin where

import Variable
import Formula
import qualified Data.Set as Set
import Data.List(sortBy,groupBy,partition)

tseitinReplaceOne :: Set.Set Variable -> Formula -> Formula -> Maybe (Formula,Variable)
tseitinReplaceOne varSet toReplace formula =
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

tseitinReplaceMany :: Set.Set Variable -> [Formula] -> Formula -> (Formula, [Variable])
tseitinReplaceMany varSet toReplaces formula
    | not (all (`containsOnlyVarsFrom` varSet) toReplaces) = error "..."
    | otherwise =
    let ordered = orderFormulas toReplaces
        withVariables = zip ordered (newVariables varSet)
        newVars = map snd withVariables
        normalized = normalize withVariables []
        replaced = foldr replace formula (reverse normalized)
    in (replaced, newVars)

newVariables :: Set.Set Variable -> [Variable]
newVariables varSet = [succ (Set.findMax varSet)..]

orderFormulas :: [Formula] -> [Formula]
orderFormulas fs =
    let groups = makeRelatedGroups fs
    in concatMap (sortBy parentChildOrdering) groups

makeRelatedGroups :: [Formula] -> [[Formula]]
makeRelatedGroups [] = []
makeRelatedGroups (f:fs) =
    let (related,others) = partition (isRelatedTo f) fs
    in (f:related) : makeRelatedGroups others

normalize :: [(Formula,Variable)] -> [(Formula,Variable)] -> [(Formula,Variable)]
normalize withVariables accum = undefined

replace :: (Formula,Variable) -> Formula -> Formula
replace (toReplace,extraVar) f = snd $ findAndReplace toReplace extraVar f

isChildOf :: Formula -> Formula -> Bool
f1 `isChildOf` f2 =
    let [unusedVar] = generateVars 1
    in fst $ findAndReplace f1 unusedVar f2

isRelatedTo :: Formula -> Formula -> Bool
isRelatedTo f1 f2 = f1 `isChildOf` f2 || f2 `isChildOf` f1

containsOnlyVarsFrom :: Formula -> Set.Set Variable -> Bool
f `containsOnlyVarsFrom` varSet = (variableSet f) `Set.isSubsetOf` varSet

parentChildOrdering :: Formula -> Formula -> Ordering
parentChildOrdering f1 f2
    | f1 == f2 = EQ
    | f1 `isChildOf` f2 = LT
    | f2 `isChildOf` f1 = GT
    | otherwise = error $ "Formulas are unrelated: " ++ show f1 ++ " and " ++ show f2
