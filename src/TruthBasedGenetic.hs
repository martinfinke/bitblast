module TruthBasedGenetic where

import TruthBasedCore(Assignment, CNF(..), Clause(..), assignments, numLiterals)
import TruthBasedApprox(Amount(..), toAbs)
import EspressoInterface
import Utils(uniqueRandom, shuffleList)
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
    initialPopulation :: Amount, -- ^ How many candidates to generate initially
    mutationAmount :: Amount, -- ^ How many candidates are mutated
    mergeAmount :: Amount,
    surviveAmount :: Amount
    }

defaultOptions = EvolutionOptions {
    initialPopulation = Percent 10,
    mutationAmount = Percent 25,
    mergeAmount = Percent 25,
    surviveAmount = Percent 10
    }

--optimize :: Int -> (Assignment -> Bool) -> Int -> IO CNF
--optimize = optimizeWith defaultOptions

--optimizeWith :: EvolutionOptions -> Int -> (Assignment -> Bool) -> Int -> IO CNF
--optimizeWith options numVars f numExtraVars = 
--    let as = assignments numVars
--        allAssignments = Set.fromList $ assignments (numVars + numExtraVars)
--        ones = filter f as
--        fitness' = fitness (numVars + numExtraVars) allAssignments ones
--        sortByFitness candidates = do
--            fitnessRatings <- forM candidates fitness'
--            let zipped = zip candidates fitnessRatings
--            return $ map fst $ sortBy (comparing snd) zipped
--        numPossibleCandidates = (2^numExtraVars - 1)^(length ones) -- TODO: I think this is wrong
--        numInitialsOrZero = toAbs (initialPopulation options) numPossibleCandidates
--        numInitials = if numInitialsOrZero == 0 then numPossibleCandidates else numInitialsOrZero
--        nextGeneration sortedCandidates =
--            let len = length sortedCandidates
--                numSurvivors = toAbs (surviveAmount options) len
--                survivors = take numSurvivors sortedCandidates
--                numMutations = toAbs (mutationAmount options) len
--                forMutation = take numMutations sortedCandidates
--                numMerges = toAbs (mergeAmount options) len
--                forMerge = take numMerges sortedCandidates
--            in do
--                mutated <- R.getStdRandom $ \rand -> mutateAll numExtraVars rand forMutation
--                merged <- R.getStdRandom $ \rand -> mergeAll rand forMerge
--                newCandidates <- fmap (take len) . sortByFitness $ survivors ++ mutated ++ merged
--                putStrLn $ "Best candidate in this generation:"
--                let best = head newCandidates
--                print best
--                rating <- fitness' best
--                putStrLn $ "with fitness: " ++ show rating
--                nextGeneration newCandidates

                
--    in do
--        putStrLn $ show numPossibleCandidates ++ " possible CNFs."
--        putStrLn $ "Starting with " ++ show numInitials ++ " initial candidates..."
--        candidates <- replicateM numInitials (getRandomCandidate ones numExtraVars)
--        sorted <- sortByFitness candidates
--        nextGeneration sorted

generation :: R.RandomGen g => [Candidate] -> Rand g [Candidate]
generation = undefined




randomCandidate :: [Assignment] -> Int -> Gen Candidate
randomCandidate ones numExtraVars = do
    selections <- replicateM (length ones) $ do
        maxNumPlacements <- choose (1, 2^numExtraVars)
        -- TODO: This is biased towards placing a small amount of ones: It only places all ones if the Arbitrary doesn't generate the same value twice.
        fmap nub $ replicateM maxNumPlacements $ vectorOf numExtraVars (arbitrary::Gen Bool)
    return . Candidate $ Map.fromList $ zip ones selections

getRandomCandidate :: [Assignment] -> Int -> IO Candidate
getRandomCandidate ones numExtraVars = fmap head $ sample' $ randomCandidate ones numExtraVars

mutate :: R.RandomGen g => [Assignment] -> Candidate -> Rand g Candidate
mutate expansions (Candidate c) =
    let list = Map.toList c
        mutateOne (base,as) = do
            let len = length as
            toAdd <- randomPlacements expansions len
            newLength <- liftRand $ R.randomR (max 1 $ len `div` 2, max 1 . round $ 1.5 * fromIntegral len)
            shuffled <- shuffleList $ as ++ toAdd
            return (base, take newLength shuffled)
    in fmap (Candidate . Map.fromList) $ forM list mutateOne

mutateAll :: R.RandomGen g => Int -> [Candidate] -> Rand g [Candidate]
mutateAll numExtraVars candidates = forM candidates $ mutate (assignments numExtraVars)

randomPlacements :: R.RandomGen g => [Assignment] -> Int -> Rand g [Assignment]
randomPlacements expansions amount = do
    shuffled <- shuffleList expansions
    return $ take amount shuffled

candidateToCNF :: Int -> Candidate -> CNF
candidateToCNF totalNumVars (Candidate m) = undefined
