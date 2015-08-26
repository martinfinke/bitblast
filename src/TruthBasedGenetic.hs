module TruthBasedGenetic where

import TruthBasedCore(Assignment, CNF(..), Clause(..), assignments, numLiterals)
import TruthBasedApprox(Amount(..), toAbs)
import EspressoInterface
import Utils(shuffleList, parallelForM)
import Test.QuickCheck
import Control.Monad
import Control.Monad.Random

import qualified System.Random as R
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord(comparing)

-- A candidate maps each f-One-Assignment to a non-empty list of (f' \ f)-Assignments.
newtype Candidate = Candidate (Map.Map Assignment [Assignment])
    deriving(Eq, Show)

merge :: R.RandomGen g => Candidate -> Candidate -> Rand g Candidate
merge (Candidate a) (Candidate b)
    | Map.keys a /= Map.keys b = error "merge error: The f-One-Assignments of both candidates have to be the same."
    | otherwise =
        let zipped = zip (Map.toAscList a) (Map.toAscList b)
        in fmap (Candidate . Map.fromList) $ forM zipped $ \(aSide,bSide) -> do
            num <- getRandom
            return $ if (num::Float) < 0.5 then aSide else bSide

mergeAll :: R.RandomGen g => [Candidate] -> Rand g [Candidate]
mergeAll candidates = do
    shuffled <- shuffleList candidates
    let partners = zip candidates shuffled
    forM partners (uncurry merge)

-- | The lower, the better.
fitness :: Int -> Set.Set Assignment -> [Assignment] -> Candidate -> IO Int
fitness totalNumVars allAssignments ones (Candidate m) =
    let expandedOnes = Set.fromList $ map expand ones
        zeros = Set.toList $ allAssignments Set.\\ expandedOnes
        expand one = concatMap (one ++) $ m Map.! one
    in do
        optimized <- espressoOptimize totalNumVars zeros
        return $ numLiterals $ CNF optimized

data EvolutionOptions = EvolutionOptions {
    populationSize :: Amount,
    mutationQuantity :: Amount, -- ^ How many candidates are mutated
    mutationAmount :: Float, -- ^ How much candidates are mutated (0 = not at all, 1 = completely random)
    mergeQuantity :: Amount,
    surviveQuantity :: Amount,
    numThreads :: Int,
    maxAge :: Int -- ^ If this many generations in sequence fail to improve, the algorithm terminates.
    }

defaultOptions = EvolutionOptions {
    populationSize = Abs 100,
    mutationQuantity = Percent 25,
    mutationAmount = 0.2,
    mergeQuantity = Percent 50,
    surviveQuantity = Percent 25,
    numThreads = 10,
    maxAge = 10
    }

--optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
--optimize = optimizeWith defaultOptions

optimizeWith :: EvolutionOptions -> Int -> (Assignment -> Bool) -> Int -> IO CNF
optimizeWith options numVars f numExtraVars =
    let ones = filter f (assignments numVars)
        numCands = numCandidates (length ones) numExtraVars
        allAssignments = Set.fromList $ assignments (numVars + numExtraVars)
        calculateFitness = fitness (numVars+numExtraVars) allAssignments ones
        lifecycle candidates (oldBest, oldBestFitness, oldAge) = do
            fitness <- parallelForM (numThreads options) $ map calculateFitness candidates
            let sortedCands = map fst $ sortBy (comparing snd) $ zip candidates fitness

            thisGenBestFitness <- calculateFitness (head sortedCands)
            (newBest, newBestFitness, age) <- if thisGenBestFitness < oldBestFitness
                then do
                    putStrLn $ "New best candidate with fitness " ++ show thisGenBestFitness ++ ":"
                    putStrLn $ show (head sortedCands)
                    return (head sortedCands, thisGenBestFitness, 0)
                else return (oldBest, oldBestFitness, oldAge+1)

            let shouldStop = age >= 10
            if shouldStop
                then return newBest
                else do
                    rand <- R.newStdGen
                    let newGeneration = flip evalRand rand $ generation options numExtraVars sortedCands
                    lifecycle newGeneration (newBest, newBestFitness, age)
    in do
        putStrLn $ "Possible candidates: " ++ show numCands
        -- TODO: create initial population, then call lifecycle
        -- Guard against the population being length 0.
        return undefined

numCandidates :: Int -> Int -> Integer
numCandidates numOnes numExtraVars = (two^(two^numExtraVars) - 1)^numOnes
    where two = 2 :: Integer


-- | The candidates must already be sorted!
generation :: R.RandomGen g => EvolutionOptions -> Int -> [Candidate] -> Rand g [Candidate]
generation options numExtraVars candidates =
    let len = length candidates
        numSurvivors = toAbs (surviveQuantity options) len
        survivors = take numSurvivors candidates
        numMutations = toAbs (mutationQuantity options) len
        forMutation = take numMutations candidates
        numMerges = toAbs (mergeQuantity options) len
        forMerge = take numMerges candidates
        numRandoms = max 0 $ len - numSurvivors - numMerges - numMutations
        ones = getOnes (head candidates)
        mutateAll' = mutateAll numExtraVars (mutationAmount options)
        randomCandidate' = randomCandidate ones numExtraVars
    in do
        merged <- mergeAll forMerge
        mutated <- mutateAll' forMutation
        randoms <- replicateM numRandoms randomCandidate'
        return . take len $ survivors ++ merged ++ mutated ++ randoms

getOnes :: Candidate -> [Assignment]
getOnes (Candidate m) = Map.keys m

randomCandidate :: R.RandomGen g => [Assignment] -> Int -> Rand g Candidate
randomCandidate ones numExtraVars = do
    let expansions = assignments numExtraVars
    m <- forM ones $ \one -> do
        numPlacements <- liftRand $ R.randomR (1, length expansions)
        placements <- randomPlacements expansions numPlacements
        return (one, placements)
    return . Candidate . Map.fromList $ m

mutate :: R.RandomGen g => [Assignment] -> Float -> Candidate -> Rand g Candidate
mutate expansions amount (Candidate c)
    | amount < 0 = error $ "mutate: amount must not be < 0: " ++ show amount
    | otherwise = fmap Candidate $ traverse mutateOne c
    where mutateOne as = do
            let len = length as
            let randomLength = round $ amount * fromIntegral len
            toAdd <- randomPlacements expansions randomLength
            let newMinLen = max 1 $ len - randomLength
            let newMaxLen = max newMinLen $ len + randomLength
            newLength <- liftRand $ R.randomR (newMinLen, newMaxLen)
            shuffled <- shuffleList $ as ++ toAdd
            return . take newLength $ nub shuffled

mutateAll :: R.RandomGen g => Int -> Float -> [Candidate] -> Rand g [Candidate]
mutateAll numExtraVars amount candidates = forM candidates $ mutate (assignments numExtraVars) amount

randomPlacements :: R.RandomGen g => [Assignment] -> Int -> Rand g [Assignment]
randomPlacements expansions amount = do
    shuffled <- shuffleList expansions
    return $ take amount shuffled

candidateToCNF :: Int -> Candidate -> CNF
candidateToCNF totalNumVars (Candidate m) = undefined
