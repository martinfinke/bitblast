module TruthBasedGenetic where

import TruthBasedCore(variableNumbers, Assignment, CNF(..), Clause(..), assignments, numLiterals, lit)
import TruthBased
import EspressoInterface
import Formula
import MinimizeFormula
import Test.QuickCheck
import Control.Monad
import Control.Monad.Random
import qualified System.Random as R
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils(shuffleList, parallelForM, Amount(..), toAbs)
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
fitness :: Int -> [Assignment] -> Candidate -> IO Int
fitness totalNumVars allAssignments cand =
    let zeros = getZeros allAssignments cand
    in do
        optimized <- espressoOptimize totalNumVars zeros
        return $ numLiterals optimized

data EvolutionOptions = EvolutionOptions {
    populationSize :: Amount,
    mutationQuantity :: Amount, -- ^ How many candidates are mutated
    mutationAmount :: Float, -- ^ How much candidates are mutated (0 = not at all, 1 = completely random)
    mergeQuantity :: Amount,
    surviveQuantity :: Amount,
    numThreads :: Int,
    maxAge :: Int, -- ^ If this many generations in sequence fail to improve, the algorithm terminates.
    printCNF :: (CNF -> IO ()), -- ^ Used to show the current best CNF
    printMessage :: (String -> IO ()) -- ^ Used to show stats about the current progress
    }

defaultOptions = EvolutionOptions {
    populationSize = Abs 100,
    mutationQuantity = Percent 25,
    mutationAmount = 0.3,
    mergeQuantity = Percent 50,
    surviveQuantity = Percent 25,
    numThreads = 10,
    maxAge = 50,
    printCNF = print,
    printMessage = putStrLn
    }

silentOptions = defaultOptions{
    printCNF = const (return ()),
    printMessage = const (return ())
    }

minimizeGenetic, minimizeGeneticSilent :: Int -> Formula -> IO Formula

minimizeGenetic numExtraVars formula = 
    let vars = Set.toAscList (variableSet formula)
        newVars' = newVars numExtraVars formula
        varsWithExtra = vars ++ newVars'
        opts = defaultOptions{printCNF = \cnf -> do
                print . prettyPrint $ fromCoreCNF varsWithExtra cnf
            }
    in minimizeGeneticWith opts numExtraVars formula

minimizeGeneticSilent = minimizeGeneticWith silentOptions

minimizeGeneticWith :: EvolutionOptions -> Int -> Formula -> IO Formula
minimizeGeneticWith options numExtraVars formula =
    let vars = Set.toAscList (variableSet formula)
        newVars' = newVars numExtraVars formula
        varsWithExtra = vars ++ newVars'
        f = toCoreFormula vars formula
    in do
        minCnf <- optimizeWith options (length vars) f numExtraVars
        return $ fromCoreCNF varsWithExtra minCnf

optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
optimize = optimizeWith defaultOptions

optimizeWith :: EvolutionOptions -> Int -> (Assignment -> Bool) -> Int -> IO CNF
optimizeWith options numVars f numExtraVars =
    let ones = filter f (assignments numVars)
        totalNumVars = numVars + numExtraVars
        expansions = assignments numExtraVars
        numCands = numCandidates (length ones) numExtraVars
        -- Prevent overflow when converting from Integer to Int:
        populationSize' = fromIntegral . max 1 . min (fromIntegral (maxBound::Int) :: Integer) $ toAbs (populationSize options) numCands :: Int
        allAssignments = assignments totalNumVars
        calculateFitness = fitness totalNumVars allAssignments
        lifecycle candidates (oldBest, oldBestFitness, oldAge) = do
            fitness <- parallelForM (numThreads options) $ map calculateFitness candidates
            let sortedCands = map fst $ sortBy (comparing snd) $ zip candidates fitness
            thisGenBestFitness <- calculateFitness (head sortedCands)
            (newBest, newBestFitness, age) <- if thisGenBestFitness < oldBestFitness
                then do
                    (printMessage options) $ "New best CNF with fitness " ++ show thisGenBestFitness ++ ":"
                    optimized <- espressoOptimize totalNumVars (getZeros allAssignments $ head sortedCands)
                    (printCNF options) optimized
                    return (optimized, thisGenBestFitness, 0)
                else return (oldBest, oldBestFitness, oldAge+1)

            let shouldStop = age > (maxAge options)
            if shouldStop
                then do
                    (printMessage options) $ "Stopping because age > " ++ show (maxAge options)
                    return newBest
                else do
                    rand <- R.newStdGen
                    let newGeneration = flip evalRand rand $ generation options numExtraVars sortedCands
                    lifecycle newGeneration (newBest, newBestFitness, age)
    in do
        (printMessage options) $ "Possible candidates: " ++ show numCands
        (printMessage options) $ "Population size: " ++ show populationSize'
        rand <- R.newStdGen
        let initialPopulation = flip evalRand rand $ replicateM populationSize' (randomCandidate expansions ones)
        let initialBest = candidateToCNF totalNumVars (head initialPopulation)
        bestCNF <- lifecycle initialPopulation (initialBest, maxBound::Int, 0)
        return $ bestCNF

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
        randomCandidate' = randomCandidate (assignments numExtraVars) ones
    in do
        merged <- mergeAll forMerge
        mutated <- mutateAll' forMutation
        randoms <- replicateM numRandoms randomCandidate'
        return . take len $ survivors ++ merged ++ mutated ++ randoms

getOnes :: Candidate -> [Assignment]
getOnes (Candidate m) = Map.keys m

getZeros :: [Assignment] -> Candidate -> [Assignment]
getZeros allAssignments (Candidate m) =
    let ones = Map.foldMapWithKey (\base expansions -> Set.fromList $ map (base++) expansions) m :: Set.Set Assignment
    in Set.toAscList $ Set.fromList allAssignments Set.\\ ones

randomCandidate :: R.RandomGen g => [Assignment] -> [Assignment] -> Rand g Candidate
randomCandidate expansions ones = do
    m <- forM ones $ \one -> do
        numPlacements <- liftRand $ R.randomR (1, max 1 $ length expansions)
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
candidateToCNF totalNumVars candidate =
    let mkClause zero = Clause . map (uncurry lit) $ zip variableNumbers zero
    in CNF $ map mkClause $ getZeros (assignments totalNumVars) candidate
