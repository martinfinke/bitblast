module Main where

import Arithmetics
import EspressoInterface
import Formula
import MinimizeFormula
import NormalForm
import Qm
import TruthTable

import System.Environment(getArgs)
import Text.Printf(printf)
import Control.Monad(forM_)
import qualified Data.Set as Set

nBitAddition :: OverflowMode -> Int -> Formula
nBitAddition overflowMode numBits =
    -- Variable ordering is [x2,x1,x0] + [x5,x4,x3] = [x8,x7,x6]
    let first = map (Atom . var) $ reverse $ [0..numBits-1]
        second = map (Atom . var) $ reverse $ [numBits..2*numBits-1]
        sums = map (Atom . var) $ reverse $ [2*numBits..3*numBits-1]
        summerCircuit = summer overflowMode first second sums
    in summerCircuit

main :: IO ()
main = do
    args <- getArgs
    let bitWidth = read (head args) :: Int
    runEspressoVerbose bitWidth
    
runVerbose :: Int -> IO ()
runVerbose numBits = do
    let addition = nBitAddition Forbid numBits
    putStrLn "Circuit:"
    putStrLn $ show addition
    putStrLn "----------------------------------------------------------------"
    let oneTerms = canonicalToBitVectors (ensureCanonical addition)
    putStrLn "Ones:"
    putStrLn $ show oneTerms
    putStrLn "----------------------------------------------------------------"
    let (numvars,ones,dc) = prepareInput oneTerms [] []
    putStrLn "(numvars,ones,dc):"
    putStrLn $ show (numvars,ones,dc)
    putStrLn "----------------------------------------------------------------"
    let primes = Set.toList $ compute_primes (Set.union ones dc)
    putStrLn "Primes:"
    putStrLn $ show $ map printAsNumbers primes
    putStrLn "----------------------------------------------------------------"
    let covers = prepareCovers primes (Set.toAscList ones)
    putStrLn "Covers:"
    putStrLn $ show covers
    putStrLn "----------------------------------------------------------------"
    let minimized = minimize_complexity numvars primes covers
    putStrLn "Minimized:"
    putStrLn $ show minimized
    putStrLn "----------------------------------------------------------------"

runEspressoVerbose :: Int -> IO ()
runEspressoVerbose numBits = do
    let addition = nBitAddition Forbid numBits
    --putStrLn "Circuit:"
    --putStrLn $ show addition
    --putStrLn "----------------------------------------------------------------"
    let onesQm = map (\one -> QmTerm(one,0)) $ canonicalToBitVectors (ensureCanonical addition)
    putStrLn $ "Ones: " ++ (show $ length onesQm)
    optimizedTerms <- espressoOptimizeExact (3*numBits) $ onesQm
    putStrLn $ "Optimized: " ++ (show $ length optimizedTerms)
    let cnf = qmTermsToFormula True (3*numBits) optimizedTerms
    putStrLn "CNF:"
    putStrLn $ show cnf
    putStrLn "----------------------------------------------------------------"
    putStrLn $ show (getStats cnf)




[x0,x1,x2,x3,x4,x5,x6,x7,x8, t1,t2] = map (Atom . var) [0..10]
-- -(0 && 1) && ((0 XOR 1) <=> 2)
oneBit = nBitAddition Forbid 1
expectedOneBit = And [Not $ And [x0,x1], Equiv [Xor [x0,x1], x2]]

-- minimal CNF without extra variables:
oneBitCnf = getFormula (toCanonicalCnf oneBit)

-- replace (0 && 1) with t1:
oneTseitin1 = And [Equiv [t1, And [x0,x1]], Not t1, Equiv [x2, Xor [x0,x1]]]

-- replace ((0 XOR 1) <=> 2) with t1:
oneTseitin2 = And [Equiv [t1, Equiv [x2, Xor [x0,x1]]], Not $ And [x0,x1], t1]

-- replace (0 XOR 1) with t1:
oneTseitin3 = And [Equiv [t1, Xor [x0,x1]], Not $ And [x0,x1], Equiv [x2,t1]]

-- replace -(0 && 1) with t1:
oneTseitin4 = And [Equiv [t1, Not $ And [x0,x1]], t1, Equiv [x2, Xor [x0,x1]]]

-- replace -(0 && 1) && ((0 XOR 1) <=> 2) with t1:
oneTseitin5 = And [Equiv [t1, And [Not $ And [x0,x1], Equiv [Xor [x0,x1], x2]]], t1]










-- replace (0 && 1) and -(0 && 1)
twoTseitin1 = And [Equiv [t1, And [x0,x1]], Equiv [t2, Not t1],
    t2, Equiv [Xor [x0,x1], x2]]

-- replace (0 && 1) and -(0 && 1) && ((0 XOR 1) <=> 2)
twoTseitin2 = And [Equiv [t1, And [x0,x1]], Equiv [t2, And [Not $ And [x0,x1], Equiv [Xor [x0,x1], x2]]],
    t2]

-- replace (0 && 1) and ((0 XOR 1) <=> 2)
twoTseitin3 = And [Equiv [t1, And [x0,x1]], Equiv [t2, Equiv [Xor [x0,x1], x2]],
    Not t1, t2]

-- replace (0 && 1) and (0 XOR 1)
twoTseitin4 = And [Equiv [t1, And [x0,x1]], Equiv [t2, Xor [x0,x1]],
    Not t1, Equiv [t2, x2]]

-- replace -(0 && 1) and -(0 && 1) && ((0 XOR 1) <=> 2)
twoTseitin5 = And [Equiv [t1, Not $ And [x0,x1]], Equiv [t2, And [t1, Equiv [Xor [x0,x1], x2]]],
    t2]

-- replace -(0 && 1) and ((0 XOR 1) <=> 2)
twoTseitin6 = And [Equiv [t1, Not $ And [x0,x1]], Equiv [t2, Equiv [Xor [x0,x1], x2]],
    t1, t2]

-- replace -(0 && 1) and (0 XOR 1)
twoTseitin7 = And [Equiv [t1, Not $ And [x0,x1]], Equiv [t2, Xor [x0,x1]],
    t1, Equiv [t2, x2]]

-- replace -(0 && 1) && ((0 XOR 1) <=> 2) and ((0 XOR 1) <=> 2)
twoTseitin8 = And [Equiv [t1, And [Not $ And [x0,x1], t2]], Equiv [t2, Equiv [Xor [x0,x1], x2]],
    t1]

-- replace -(0 && 1) && ((0 XOR 1) <=> 2) and (0 XOR 1)
twoTseitin9 = And [Equiv [t1, And [Not $ And [x0,x1], Equiv [t2, x2]]], Equiv [t2, Xor [x0,x1]],
    t1]

-- replace ((0 XOR 1) <=> 2) and (0 XOR 1)
twoTseitin10 = And [Equiv [t1, Equiv [t2, x2]], Equiv [t2, Xor [x0,x1]],
    And [Not $ And [x0,x1], t1]]

twoExtraVariables = [
    twoTseitin1,
    twoTseitin2,
    twoTseitin3,
    twoTseitin4,
    twoTseitin5,
    twoTseitin6,
    twoTseitin7,
    twoTseitin8,
    twoTseitin9,
    twoTseitin10
    ]

twoExtraMinimal = map minimizeFormula twoExtraVariables







threeBitAdditionExpected = And [Not (Or [And [x5,Or [And [x4,And [x0,x3]],And [x1,And [x0,x3]],And [x1,x4]]],And [x2,Or [And [x4,And [x0,x3]],And [x1,And [x0,x3]],And [x1,x4]]],And [x2,x5]]),Equiv [Xor [x2,x5,Or [And [x4,And [x0,x3]],And [x1,And [x0,x3]],And [x1,x4]]],x8],Equiv [Xor [x1,x4,And [x0,x3]],x7],Equiv [Xor [x0,x3],x6]]