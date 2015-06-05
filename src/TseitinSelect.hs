module TseitinSelect where

import Formula
import Data.List(nub)

possibleReplacements :: Formula -> [Formula]
possibleReplacements = nub . possibleReplacements'

possibleReplacements' :: Formula -> [Formula]
possibleReplacements' formula = case formula of
    Atom _ -> [formula]
    Not f -> Not f : possibleReplacements f
    And fs -> thisAnd fs
    Or fs -> thisAnd fs
    Implies premise conclusion -> thisAnd [premise,conclusion]
    Xor fs -> thisAnd fs
    Equiv fs -> thisAnd fs
    where thisAnd fs = formula : concatMap possibleReplacements fs
