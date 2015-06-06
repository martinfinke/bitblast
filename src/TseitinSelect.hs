module TseitinSelect where

import Formula
import Data.List(nub,sort)
import Control.Monad
import qualified Control.Monad.State.Lazy as State

data SelectOptions = SelectOptions {
    includeRoot::Bool, -- ^ Include the root in the results?
    includeAtoms::Bool, -- ^ Only relevant if includeLiterals is True.
    includeLiterals::Bool -- ^ If set to False, value of includeAtoms will be ignored.
    }

-- | The default 'SelectOptions'
selectOptions :: SelectOptions
selectOptions = SelectOptions {
    includeRoot = True,
    includeAtoms = True,
    includeLiterals = True
    }

possibleReplacements :: Formula -> [Formula]
possibleReplacements = possibleReplacementsWith selectOptions

possibleReplacementsWith, possibleReplacementsWith' :: SelectOptions -> Formula -> [Formula]
possibleReplacementsWith options f = nub $ possibleReplacementsWith' options f

possibleReplacementsWith' options formula = case formula of
    Atom _ -> if includeRoot options && includeLiterals options && includeAtoms options
                then [formula]
                else []
    Not f -> if includeRoot options && (includeLiterals options || not (isLiteral f))
                then Not f : possibleReplacementsWith newOptions f
                else possibleReplacementsWith newOptions f
    And fs -> thisAnd fs
    Or fs -> thisAnd fs
    Implies premise conclusion -> thisAnd [premise,conclusion]
    Xor fs -> thisAnd fs
    Equiv fs -> thisAnd fs
    where thisAnd fs = if includeRoot options then formula : rest fs else rest fs
          rest fs = concatMap (possibleReplacementsWith newOptions) fs
          newOptions = options{includeRoot=True}

possibleReplacements2 :: Formula -> [(Formula,Formula)]
possibleReplacements2 = possibleReplacements2With selectOptions

possibleReplacements2With options formula = 
    let possibilities = possibleReplacementsWith options formula
    in [(f1,f2) | f1 <- possibilities, f2 <- possibilities]

combinationsNoMirror :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinationsNoMirror i ls = reverse $ combinationsNoMirror' i ls

combinationsNoMirror' :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinationsNoMirror' 0 _ = [[]]
combinationsNoMirror' i xs = 
    let forOne x xs' = map (x:) $ combinationsNoMirror' (i-1) xs'
        forAll = foldr (\_ (accum, (x:xs')) ->
                            let current = forOne x xs'
                            in (current:accum, xs')
                        ) ([], xs) xs
    in concat . fst $ forAll