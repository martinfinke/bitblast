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
import qualified Data.Map.Strict as Map
import Numeric(showEFloat)

import Utils(shuffleList, parallelForM, Amount(..), toAbs)
import Data.List
import Data.Ord(comparing)
import Data.Time.Clock

-- A candidate maps each f-One-Assignment to a non-empty list of (f' \ f)-Assignments.
newtype Candidate = Candidate (Map.Map Assignment [Assignment])
    deriving(Eq, Show, Ord)

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
    allowedReshuffles :: Maybe Int, -- ^ How many restarts are allowed. 'Just 0' means none at all, 'Nothing' means infinitely many.
    reshuffleAge :: Maybe Int, -- ^ If this many generations in sequence fail to improve, the algorithm will restart with a new, completely shuffled population. If 'Nothing', it will never reshuffle and run indefinitely.
    printCNF :: (CNF -> IO ()), -- ^ Used to show the current best CNF
    printMessage :: (String -> IO ()) -- ^ Used to show stats about the current progress
    }

defaultOptions = EvolutionOptions {
    populationSize = Abs 200,
    mutationQuantity = Percent 30,
    mutationAmount = 0.5,
    mergeQuantity = Percent 50,
    surviveQuantity = Percent 20,
    numThreads = 10,
    allowedReshuffles = Just 20,
    reshuffleAge = Just 50,
    printCNF = \cnf -> do t <- getCurrentTime; putStrLn $ show t ++ ": " ++ show cnf,
    printMessage = \msg -> do t <- getCurrentTime; putStrLn $ show t ++ ": " ++ msg
    }

silentOptions = defaultOptions{
    printCNF = const (return ()),
    printMessage = const (return ())
    }

minimizeGenetic, minimizeGeneticSilent, minimizeGeneticEndless :: Int -> Formula -> IO Formula

minimizeGenetic = minimizeGeneticPretty False
minimizeGeneticEndless = minimizeGeneticPretty True

minimizeGeneticPretty endless numExtraVars formula = 
    let vars = Set.toAscList (variableSet formula)
        newVars' = newVars numExtraVars formula
        varsWithExtra = vars ++ newVars'
        opts = defaultOptions{
            printCNF = \cnf -> do
                t <- getCurrentTime;
                putStrLn $ show t ++ ":"
                print . prettyPrint $ fromCoreCNF varsWithExtra cnf,
            allowedReshuffles = if endless then Nothing else (allowedReshuffles defaultOptions)
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
        print' = (printMessage options)
        totalNumVars = numVars + numExtraVars
        expansions = assignments numExtraVars
        numCands = numCandidates (length ones) numExtraVars
        -- Prevent overflow when converting from Integer to Int:
        populationSize' = fromIntegral . max 1 . min (fromIntegral (maxBound::Int) :: Integer) $ toAbs (populationSize options) numCands :: Int
        allAssignments = assignments totalNumVars
        calculateFitness mem cand = case Map.lookup cand mem of
                Just rating -> return rating
                Nothing -> fitness totalNumVars allAssignments cand
        lifecycle candidates (oldBest, oldBestFitness, oldAge, doneRestarts) fitnessMemory = do
            fitness <- parallelForM (numThreads options) $ map (calculateFitness fitnessMemory) candidates
            let zipped = zip candidates fitness
            let sortedCands = map fst $ sortBy (comparing snd) $ zipped
            let fitnessMemory' = Map.union fitnessMemory $ Map.fromList zipped
            thisGenBestFitness <- calculateFitness fitnessMemory' (head sortedCands)
            (newBest, newBestFitness, age) <- if thisGenBestFitness < oldBestFitness
                then do
                    print' $ "New best CNF with fitness " ++ show thisGenBestFitness ++ ":"
                    optimized <- espressoOptimize totalNumVars (getZeros allAssignments $ head sortedCands)
                    (printCNF options) optimized
                    return (optimized, thisGenBestFitness, 0)
                else return (oldBest, oldBestFitness, oldAge+1)

            let shouldStop = case (allowedReshuffles options) of
                    Nothing -> False
                    Just allowedReshuffles' -> doneRestarts > allowedReshuffles'
            if shouldStop
                then do
                    print' $ "Stopping because no more restarts are allowed."
                    return newBest
                else do
                    rand <- R.newStdGen
                    let shouldReshuffle = case (reshuffleAge options) of
                            Nothing -> False
                            Just reshuffleAge' -> oldAge >= reshuffleAge'
                    if shouldReshuffle
                        then reshufflePopulation (newBest, newBestFitness, age, doneRestarts) fitnessMemory' rand
                        else let newGeneration = flip evalRand rand $ generation options numExtraVars sortedCands
                             in lifecycle newGeneration (newBest, newBestFitness, age, doneRestarts) fitnessMemory'
        reshufflePopulation (best, bestFitness, _, doneRestarts) fitnessMemory rand = do
            --print' $ "Shuffling population..."
            let randomPopulation = flip evalRand rand $ replicateM populationSize' (randomCandidate expansions ones)
            lifecycle randomPopulation (best, bestFitness, 0, doneRestarts + 1) fitnessMemory
    in do
        print' $ "Possible candidates: " ++ showNumCandidates numCands
        print' $ "Population size: " ++ show populationSize'
        withoutExtra <- espressoOptimize numVars $ filter (not . f) (assignments numVars)
        let numLitsWithoutExtra = numLiterals withoutExtra
        print' $ "CNF without extra variables has " ++ show numLitsWithoutExtra ++ " literals:"
        (printCNF options) withoutExtra
        rand <- R.newStdGen
        bestCNF <- reshufflePopulation (withoutExtra, numLitsWithoutExtra, 0, 0) Map.empty rand
        return $ bestCNF

numCandidates :: Int -> Int -> Integer
numCandidates numOnes numExtraVars = (two^(two^numExtraVars) - 1)^numOnes
    where two = 2 :: Integer

showNumCandidates :: Integer -> String
showNumCandidates num =
    let outputLength = length $ show num
        approx = showEFloat (Just 1) (fromInteger num :: Double) ""
    in if outputLength > 10 then approx else show num


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
