module TseitinSelect where

import Formula
import Data.List(nub)

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
possibleReplacements = nub . possibleReplacementsWith selectOptions

possibleReplacementsWith :: SelectOptions -> Formula -> [Formula]
possibleReplacementsWith options formula = case formula of
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