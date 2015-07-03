{-# language FlexibleContexts #-}
module TruthBased2 where

import Satchmo.Counting.Binary as C
import Satchmo.SAT.Mini
import Satchmo.Boolean
import Satchmo.Code

import Variable
import QmcTypes
import ModifiedQmc hiding (merge)
import qualified Data.Set as Set

type Constraints = ([[Int]], [(Int, [Int])], [Int], [[Int]])

foo :: TruthTable -> Set.Set Variable -> Constraints
foo truthTable extraVars =
    let allForExtraVars = allBoolCombinations extraVars
        (trues, falses) = trueAndFalse truthTable
        expansionsForEachTrue = [map (merge true) allForExtraVars | true <- trues] :: [[Assignment]]
        concatExpansionsForEachTrue = concat expansionsForEachTrue
        expansionsForEachFalse = concat [map (merge false) allForExtraVars | false <- falses] :: [Assignment]
        primes = modifiedQmc (map assignmentToBitVector expansionsForEachFalse) (map assignmentToBitVector $ concatExpansionsForEachTrue) :: [QmTerm]
        constraints = prepareConstraints expansionsForEachTrue expansionsForEachFalse primes

    in constraints

prepareConstraints :: [[Assignment]] -> [Assignment] -> [QmTerm] -> Constraints
prepareConstraints expansionsForEachTrue expansionsForEachFalse primes =
    let expandSize = case expansionsForEachTrue of
                [] -> 0
                (x:_) -> length x
        concatExpansionsForEachTrue = concat expansionsForEachTrue
        (os, ps) = makeIndices concatExpansionsForEachTrue expansionsForEachFalse primes
        atLeastOnePrimeForEachFalse = [[j | (prime, j) <- zip primes ps, primeCoversOne prime (assignmentToBitVector false)] | false <- expansionsForEachFalse] :: [[Int]]
        noPrimeCoversAOne = [(i, [j | (prime, j) <- zip primes ps, primeCoversOne prime (assignmentToBitVector true)]) | (i, true) <- zip os $ concatExpansionsForEachTrue] :: [(Int, [Int])]
        allFalsesRemainFalse = take (length expansionsForEachFalse) $ drop (length concatExpansionsForEachTrue) $ os :: [Int]
        atLeastOneExpansionIsTrue = fst $ foldr (\_ (list, os') -> (take expandSize os : list, drop expandSize os')) ([], os) expansionsForEachTrue :: [[Int]]
    in (atLeastOnePrimeForEachFalse, noPrimeCoversAOne, allFalsesRemainFalse, atLeastOneExpansionIsTrue)

makeIndices :: [Assignment] -> [Assignment] -> [QmTerm] -> ([Int], [Int])
makeIndices concatExpansionsForEachTrue expansionsForEachFalse primes =
    let os = [0 .. (length concatExpansionsForEachTrue + length expansionsForEachFalse)-1]
        ps = [length os .. (length os + length primes)-1]
    in (os, ps)

-- TODO: Take care of CNF/DNF format!
assignmentToBitVector :: Assignment -> BitVector
assignmentToBitVector = undefined