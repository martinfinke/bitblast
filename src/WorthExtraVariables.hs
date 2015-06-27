module WorthExtraVariables where

import qualified Data.Set as Set
import Variable
import Utils(combinationsNoMirror)

allPossibleTables :: Set.Set Variable -> [TruthTable]
allPossibleTables varSet =
    let allFalse = allFalseTable varSet
        assignments = allAssignments allFalse
        assignmentSets = concat [combinationsNoMirror len assignments | len <- [0..length assignments]]
        setAssignmentsToTrue assignments = foldr (\assignment table -> setRow assignment True table) allFalse assignments
    in map setAssignmentsToTrue assignmentSets