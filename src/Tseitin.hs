module Tseitin where

import Variable
import Formula
import qualified Data.Set as Set
import Data.List(sortBy,partition)
import Data.Maybe(catMaybes)
import Utils(mapWithRest)
import Debug.Trace(traceShow)

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

tseitinReplace :: Set.Set Variable -> [Formula] -> Formula -> (Formula, [Variable])
tseitinReplace varSet toReplaces formula
    | null toReplaces = (formula,[])
    | not (all (`containsOnlyVarsFrom` varSet) toReplaces) = error "The terms to replace must contain only variables from the variable set. Otherwise, the introduced extra variables might clash."
    | otherwise =
    let hierarchical = reverse . concat . map Set.toList $ hierarchy (Set.fromList toReplaces)
        newVars = take (length hierarchical) (newVariables varSet)
        withVariables = zip hierarchical newVars
        normalized = normalize withVariables
        replaced = foldr replace formula (reverse normalized)
        equivs = map makeEquiv normalized
    in (And (replaced:equivs), newVars)

makeEquiv :: (Formula,Variable) -> Formula
makeEquiv (f,variable) = Equiv [Atom variable, f]

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

normalize :: [(Formula,Variable)] -> [(Formula,Variable)]
normalize = reverse . foldr normalize' [] . reverse

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

hierarchy :: Set.Set Formula -> [Set.Set Formula]
hierarchy remainingTerms
    | Set.null remainingTerms = []
    | otherwise = 
        let currentLevel = Set.filter (\t -> not $ any (\t' -> t /= t' && t `isChildOf` t') remainingTerms) remainingTerms
            rest = Set.difference remainingTerms currentLevel
        in currentLevel : hierarchy rest