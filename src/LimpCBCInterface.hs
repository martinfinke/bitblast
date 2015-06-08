module LimpCBCInterface where

import QmcTypes
import qualified Data.Map as Map





import Numeric.Limp.Rep
import Numeric.Limp.Program
import Numeric.Limp.Program.ResultKind

import Numeric.Limp.Solvers.Cbc


columns :: [QmTerm] -> [BitVector] -> Map.Map BitVector [Int]
columns primes ones =
    let indexedPrimes = zip [0..] primes
        cols = Map.fromList [(one, [i | (i,prime) <- indexedPrimes, primeCoversOne prime one]) | one <- ones]
        uncoveredMinterms = Map.filter null cols
    in if Map.null uncoveredMinterms
        then cols
        else error $ "Uncovered minterms: " ++ show (Map.keys uncoveredMinterms)



toLimpProgram :: Int -> [QmTerm] -> [BitVector] -> Program Int Int IntDouble
toLimpProgram numVars primes ones =
    let cols = columns primes ones
        mask = invertedNumvarsMask numVars
        prices = map (Z . primeComplexity mask) primes
        varNames = variableNames primes
        withPrices = zipWith z varNames prices
        objective = foldr (.+.) (head withPrices) (tail withPrices)
        constraintList = Map.foldr (\primes list ->
                    let terms = map z1 primes
                        rhs = con 1
                        lhs = foldr (.+.) (head terms) (tail terms)
                        line = lhs :>= rhs
                    in line:list
                    ) [] cols
        constraints = foldr (:&&) (head constraintList) (tail constraintList)
        binaryBounds = map (\i -> lowerUpperZ 0 i 1) varNames
    in program Minimise objective constraints binaryBounds :: Program Int Int IntDouble

runLimpCBC :: Int -> [QmTerm] -> [BitVector] -> [QmTerm]
runLimpCBC numVars primes ones =
    let program = toLimpProgram numVars primes ones
        solution = solve program
        essentialPrimeIndices = case solution of
            Left _ -> error "No solution."
            Right assignment -> filter (\i -> zOf assignment i /= 0) (variableNames primes)
        essentialPrimes = map snd $ filter (\(i,_) -> i `elem` essentialPrimeIndices) $ zip [0..] primes
    in essentialPrimes


variableNames :: [QmTerm] -> [Int]
variableNames primes = take (length primes) [0..]

