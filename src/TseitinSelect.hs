module TseitinSelect where

import Formula
import Data.List(nub,sort,sortBy)
import Data.Ord(comparing)
import Utils(combinationsNoMirror)

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

possibleReplacementsNWith :: Int -> SelectOptions -> Formula -> [[Formula]]
possibleReplacementsNWith len options f =
    let replacements = possibleReplacementsWith options f
        combinations = combinationsNoMirror len replacements
    in combinations

possibleReplacementsSorted :: Formula -> [Formula]
possibleReplacementsSorted f =
    let replacements = possibleReplacements f
    in sortBy (comparing $ negate . numOccurrences f) replacements

numOccurrences :: Formula -> Formula -> Int
numOccurrences f term = numOccurrences' term 0 f

numOccurrences' :: Formula -> Int -> Formula -> Int
numOccurrences' term num formula = case formula of
    Atom _ -> current
    Not f -> numOccurrences' term current f
    And fs -> current + numInChildren fs
    Or fs -> current + numInChildren fs
    Implies prem conc -> current + numInChildren [prem, conc]
    Xor fs -> current + numInChildren fs
    Equiv fs -> current + numInChildren fs
    where current = if term == formula then num+1 else num
          numInChildren fs = sum . map (numOccurrences' term 0) $ fs

