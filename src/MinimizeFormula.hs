module MinimizeFormula where

import LimpCBCInterface
import Formula
import NormalForm
import qualified Data.Set as Set
import qualified Data.Bits as B
import Data.List(sortBy, minimumBy)
import Data.Ord(comparing)
import QmcCpp
import QmcTypes
import SatchmoInterface
import Variable
import Tseitin
import TseitinSelect
import TruthBasedNaive
import VariableSpec
import Utils

import Control.Monad(when, forM, foldM)
import System.Info(os)
import System.Process(readProcess)
import System.IO.Unsafe
import Test.Hspec
import Test.QuickCheck

data MinimizeFormulaOptions = MinimizeFormulaOptions {
    useSolver :: Int -> [QmTerm] -> [BitVector] -> IO [QmTerm],
    verboseOutput :: Bool,
    verifyPrimes :: Bool,
    verifyResult :: Bool
    }

defaultMinimizeFormulaOptions :: MinimizeFormulaOptions
defaultMinimizeFormulaOptions = MinimizeFormulaOptions {
    useSolver = runLimpCBC,
    verboseOutput = False,
    verifyPrimes = False,
    verifyResult = False
}

minimizeFormulaWith :: MinimizeFormulaOptions -> Formula -> IO Formula
minimizeFormulaWith options formula =
    let canonical = ensureCanonical formula
        cnfMode = (getType canonical == CNFType)
        varSet = variableSet formula
        numVars = Set.size varSet
        ones = canonicalToBitVectors varSet canonical
    in do
        when (verboseOutput options) $ putStrLn $ show (length ones) ++ " ones."
        primes <- qmcCppComputePrimes ones
        when (verboseOutput options) $ putStrLn $ "Found " ++ show (length primes) ++ " primes."

        when (verifyPrimes options) $ do
            putStrLn "Verifying Primes Formula..."
            let primesFormula = qmTermsToFormula varSet cnfMode primes
            quickCheck $ property $ \assignment ->
                let assignment' = expandOrReduce False varSet assignment
                in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` primesFormula
        
        essentialPrimes <- (useSolver options) numVars primes ones
        let cnf = qmTermsToFormula varSet cnfMode essentialPrimes

        when (verifyResult options) $ do
            putStrLn "Validating minimized Formula..."
            quickCheck $ property $ \assignment ->
                let assignment' = expandOrReduce False varSet assignment
                in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` cnf

        return cnf

minimizeFormula :: Formula -> IO Formula
minimizeFormula = minimizeFormulaWith defaultMinimizeFormulaOptions


minimizeWithNExtraVars :: Int -> Formula -> IO (Formula, [Variable])
minimizeWithNExtraVars numExtraVars f =
    let varSet = variableSet f
        possibilities = possibleReplacementsNWith numExtraVars selectOptions f
        replaced = map (\rs -> tseitin varSet rs f) possibilities :: [TseitinFormula]
    in do
        optimized <- forM replaced $ \(TseitinFormula f newVars equivTerms) -> do
            And clauses <- minimizeFormula f
            equivClauses <- fmap (concat . map removeAnd) $ forM equivTerms minimizeFormula
            return (And (clauses ++ equivClauses), newVars, equivTerms)
        let (f,vars,equivs) = minimumBy (comparing $ \(f,_,_) -> numLiterals $ getStats f) optimized
        print equivs
        return (f,vars)
    where removeAnd (And fs) = fs

minimizeWithExtraVarRange :: (Int,Int) -> Formula -> IO (Formula, [Variable])
minimizeWithExtraVarRange (min',max') f = do
    bestForEveryAllowedNumber <- forM [min'..max'] (flip minimizeWithNExtraVars f)
    return $ minimumBy byNumLiterals bestForEveryAllowedNumber
            

byNumLiterals = comparing $ numLiterals . getStats . fst

canonicalToBitVectors :: Set.Set Variable -> Canonical -> [BitVector]
canonicalToBitVectors varSet canonical = concatMap (convertDashes . packTerm varSet) terms
    where terms = normalFormChildren formula
          formula = getFormula canonical

-- | Replaces every dash ('Nothing') in the elements of a 'QmTerm' with two instances of the term: One with a 0, and one with a 1. This is done for all possible combinations, so if there are n dashes in a term, the output list has a length of 2^n.
convertDashes :: QmTerm -> [BitVector]
convertDashes qmTerm = foldr forEachBit [term] [0..B.finiteBitSize term - 1]
    where term = getTerm qmTerm
          mask = getMask qmTerm
          forEachBit pos bvs
                | B.testBit mask pos = map (flip B.setBit pos) bvs ++ map (flip B.clearBit pos) bvs
                | otherwise = bvs

-- | Extracts the value of a 'Variable' in a term. Used to convert terms to 'QmcTerm's. If the input 'Formula' is a CNF, the values are inverted (i.e. a positive literal results in a 0).
valueForVariable :: Formula -> Variable -> Maybe Bool
valueForVariable term variable
    | Atom variable `elem` literals = Just $ if cnfMode then False else True
    | Not (Atom variable) `elem` literals = Just $ if cnfMode then True else False
    | otherwise = Nothing
    where literals = normalFormChildren term
          cnfMode = case term of
                Or _ -> True
                And _ -> False

-- | Converts a list of 'QmTerm's back to a 'Formula'
qmTermsToFormula :: Set.Set Variable -- ^ The variable set of the formula
                 -> Bool -- ^ Whether the 'Formula' was a CNF. If so, the terms will be inverted.
                 -> [QmTerm]
                 -> Formula
qmTermsToFormula varSet cnfMode qmTerms =
    let terms = map (unpackTerm cnfMode varSet) qmTerms
        rootOp = if cnfMode then And else Or
    in  rootOp terms

packTerm :: Set.Set Variable -> Formula -> QmTerm
packTerm varSet term =
    let emptyQmTerm = (QmTerm (0,0))
        appearsInTerm = flip Set.member varSet
        selection = filter appearsInTerm (Set.toAscList varSet)
        variablesWithPositions = zip [0..] selection
        setBitForVariable (i,variable) (QmTerm (bv,mask)) = QmTerm $ case valueForVariable term variable of
            Nothing -> (bv, B.setBit mask i)
            Just bool -> (if bool then B.setBit bv i else B.clearBit bv i, mask)
    in foldr setBitForVariable emptyQmTerm variablesWithPositions

unpackTerm :: Bool -> Set.Set Variable -> QmTerm -> Formula
unpackTerm cnfMode varSet (QmTerm (term,mask)) =
    let appearsInTerm = flip Set.member varSet
        selection = filter appearsInTerm (Set.toAscList varSet)
        variablesWithPositions = zip [0..] selection
        op = if cnfMode then Or else And
        invertIfCnf v b = if cnfMode == b then Not (Atom v) else Atom v
        setVariableForBit (i,variable) restLiterals
              | B.testBit mask i = restLiterals
              | otherwise = invertIfCnf variable (B.testBit term i) : restLiterals
    in op $ foldr setVariableForBit [] variablesWithPositions
