module Tseitin where

import Variable
import Formula
import qualified Data.Set as Set
import Data.List(sortBy,partition)

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
    | null toReplaces = (formula,[])
    | not (all (`containsOnlyVarsFrom` varSet) toReplaces) = error "The terms to replace must contain only variables from the variable set. Otherwise, the introduced extra variables might clash."
    | otherwise =
    let groups = orderedGroups toReplaces
        newVars = take (length toReplaces) (newVariables varSet)
        withVariables = fst $ foldr assignVariables ([], newVars) groups
        normalized = concatMap normalize withVariables
        --replaced = foldr replace formula (reverse withVariables)
        replaced = undefined
    in (replaced, newVars) -- TODO: Add the Equiv terms

newVariables :: Set.Set Variable -> [Variable]
newVariables varSet = [succ (Set.findMax varSet)..]

relatedGroups :: [Formula] -> [[Formula]]
relatedGroups [] = []
relatedGroups (f:fs) =
    let (related,others) = partition (isRelatedTo f) fs
    in (f:related) : relatedGroups others

orderedGroups :: [Formula] -> [[Formula]]
orderedGroups = map (sortBy parentChildOrdering) . relatedGroups

assignVariables :: [Formula] -> ([[(Formula,Variable)]], [Variable]) -> ([[(Formula,Variable)]], [Variable])
assignVariables group (accum, unusedVars) =
    let withVars = zip group unusedVars
    in (withVars:accum, drop (length group) unusedVars)

-- we then "normalize" the toReplace terms. starting with the second-but-lowest one, we check if it contains any of the lower terms. if so, we replace any occurrences with the extra variable of the lower term. we then proceed to the next greater term, checking if it contains any lower (already normalized) terms.
normalize :: [(Formula,Variable)] -> [(Formula,Variable)]
normalize [] = []
normalize group = reverse $ foldr normalize' [] (reverse group)

normalize' :: (Formula,Variable) -> [(Formula,Variable)] -> [(Formula,Variable)] 
normalize' current alreadyNormalized =
    let replace' (r,v) (t,v') = (replace (r,v) t, v')
        replaced = foldr replace' current alreadyNormalized :: (Formula,Variable)
    in replaced:alreadyNormalized


replace :: (Formula,Variable) -> Formula -> Formula
replace (toReplace,extraVar) f = snd $ findAndReplace toReplace extraVar f

isChildOf :: Formula -> Formula -> Bool
f1 `isChildOf` f2 =
    let [unusedVar] = makeVars 1
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