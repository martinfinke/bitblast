module TruthBasedApprox where

import TruthBased
import qualified TruthBasedCore as Core

import qualified System.Random as R
import Control.Monad
import Data.Maybe
import Data.List(nub, sortBy)
import Data.Ord(comparing)
import Utils
import Formula
import MinimizeFormula
import NormalForm



data ClauseSelector = RandomSelector (R.StdGen -> Amount -> [Core.Clause] -> (R.StdGen, [Core.Clause]))
                    | Selector (Amount -> [Core.Clause] -> [Core.Clause])

uniformRandom :: ClauseSelector
uniformRandom = RandomSelector $ \rand amount clauses -> 
    let len = length clauses
        numClauses = toAbs amount len
        randomValues = R.randoms rand :: [Float]
        toIndex = floor . (*) (fromIntegral len) :: Float -> Int
        (newRand, indices) = uniqueRandom toIndex rand numClauses
    in (newRand, indexed indices clauses)

onlySmall :: ClauseSelector
onlySmall = Selector $ \amount clauses ->
    let sorted = sortBy (comparing $ \(Core.Clause lits) -> length lits) clauses
    in take (toAbs amount $ length clauses) sorted

minimizeTruthBasedApprox :: Amount -> Int -> Formula -> IO ()
minimizeTruthBasedApprox amount = minimizeTruthBasedApproxWith bestOptions uniformRandom amount

minimizeTruthBasedApproxWith  :: Core.Options -> ClauseSelector -> Amount -> Int -> Formula -> IO ()
minimizeTruthBasedApproxWith options selector amount numExtraVars formula =
    let vars = variables formula
        numVars = length vars
        f = toCoreFormula vars formula
        table = Core.tableWith options numVars f numExtraVars
        (Core.Table _ allClauses _) = if Core.removeIllegals options
            then Core.removeIllegalClauses numVars table
            else table
        minimize cls bestKnownValue = Core.optimizeParallel options bestKnownValue numVars f numExtraVars cls
        shuffleAndMinimize rand bestKnownValue = do
            guard $ bestKnownValue > 0

            let (maybeNewRand,selection) = case selector of
                    RandomSelector s -> let (newRand, sel) = s rand amount allClauses in (Just newRand, sel)
                    Selector s -> (Nothing, s amount allClauses)
            putStrLn "Minimizing with clause set:"
            print selection
            result <- minimize selection bestKnownValue
            when (isJust result) $ do
                let cnf = fromCoreCNF (vars ++ newVars numExtraVars formula) $ fromJust result
                let numLits = numLiterals $ getStats cnf
                putStrLn $ "Found CNF with " ++ (show numLits) ++ " literals:"
                print cnf
            when (isJust maybeNewRand) $ shuffleAndMinimize (fromJust maybeNewRand) bestKnownValue

    in do
        withoutExtraVariables <- minimizeFormula formula
        let numLits = numLiterals . getStats $ withoutExtraVariables
        rand <- R.getStdGen
        shuffleAndMinimize rand numLits
