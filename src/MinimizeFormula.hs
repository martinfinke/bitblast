module MinimizeFormula where

import CoinCBCInterface
import Formula
import NormalForm
import qualified Data.Set as Set
import qualified Data.Bits as B
import QmcCpp
import QmcTypes
import Variable

import Control.Monad(when)
import System.Info(os)
import System.Process(readProcess)
import Test.Hspec
import Test.QuickCheck
import VariableSpec

minimizeFormula :: Formula -> IO Formula
minimizeFormula formula =
    let canonical = ensureCanonical formula
        cnfMode = (getType canonical == CNFType)
        varSet = variableSet formula
        numVars = Set.size varSet
        ones = canonicalToBitVectors varSet canonical
    in do
        putStrLn $ show (length ones) ++ " ones."
        let primes = qmcCppComputePrimes ones
        putStrLn $ "Found " ++ show (length primes) ++ " primes."
        putStrLn "Validating that a formula created from the primes has the same value as the original one (for random assignments)..."

        let primesFormula = qmTermsToFormula varSet cnfMode primes
        quickCheck $ property $ \assignment ->
            let assignment' = expandOrReduce False varSet assignment
            in assignment' `isModelOf` formula `shouldBe` assignment' `isModelOf` primesFormula

        let lpFileContents = toLPFile numVars primes ones
        let lpFileName = "bitblast-cbctemp.lp"
        writeFile lpFileName lpFileContents
        let cbcSolutionFileName = "bitblast-cbcsolution.txt"
        let cbcCommands = "import " ++ lpFileName ++ "\nbranchAndCut" ++ "\nsolution " ++ cbcSolutionFileName ++ "\n"
        putStrLn "Run CBC in the current working directory and execute these commands:"
        putStrLn ""
        putStrLn cbcCommands
        copyToClipboard cbcCommands
        putStrLn "After the solution file has been written, press Enter to continue ..."
        getLine
        cbcSolution <- readFile cbcSolutionFileName
        let essentialPrimeIndices = parseSolution cbcSolution
        let essentialPrimes = map snd $ filter (\(i,_) -> i `elem` essentialPrimeIndices) $ zip [0..] primes
        return $ qmTermsToFormula varSet cnfMode essentialPrimes


-- | Only on OSX for now
copyToClipboard :: String -> IO ()
copyToClipboard str = do
    when (os == "darwin") $ do
        readProcess "pbcopy" [] str
        putStrLn "The commands have been copied to the clipboard."

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