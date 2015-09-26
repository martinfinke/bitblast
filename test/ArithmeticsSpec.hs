module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import NormalForm
import Variable hiding (eval)
import qualified Data.Set as Set
import ArithmeticTruthTables
import Control.Monad
import Assignment
import TruthTable
import ParseCnf

spec :: Spec
spec = do
    let trueOnlyForAssignments varSet assignments = foldr (flip setRow True) (allFalseTable varSet) assignments
    let mkVars number = let vars = makeVars number in (map Atom vars, Set.fromList vars)
    describe "halfAdder" $ do
        it "has the correct truth table" $ do
            let ([c,s,y,x],varSet) = mkVars 4
            let ha = halfAdder (x,y) (s,c)
            let trueAssignments = map (assignmentFromString varSet) [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            toTruthTable ha `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "fullAdderSegment" $ do
        let ([s,cOut,cIn,y,x],varSet) = mkVars 5
        let (s',cOut') = fullAdderSegment (x,y) cIn
        let connectedOutputs = And [Equiv [s,s'], Equiv [cOut,cOut']]
        it "has the correct truth table when the outputs are connected" $ do
            let trueAssignments = map (assignmentFromString varSet) [
                    "00000",
                    "00101",
                    "01001",
                    "01110",
                    "10001",
                    "10110",
                    "11010",
                    "11111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "is equivalent to a fullAdder when the outputs are connected" $ do
            let fa = fullAdder (x,y) (cIn,cOut) s
            toTruthTable connectedOutputs `shouldBe` toTruthTable fa

    describe "summerSegment" $ do
        it "is equivalent to a fullAdderSegment for 1 bit" $ do
            let ([y,x],varSet) = mkVars 2
            let (s', cOut') = halfAdderSegment (x,y)
            let (s'', cOut'') = summerSegment Nothing [x] [y]
            let equiv = And [Equiv [s', head s''], Equiv [cOut', cOut'']]
            toTruthTable equiv `shouldBe` allTrueTable varSet

        it "has the correct truth table for 2 bits" $ do
            let ([cOut,s0,s1,y0,y1,x0,x1],varSet) = mkVars 7
            
            let ([s1',s0'], cOut') = summerSegment Nothing [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Equiv [cOut, cOut']]
            let smer = summer (Connect cOut) [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString varSet) addition2BitConnectOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments varSet trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "summer" $ do
        it "has the correct truth table for 1 bit" $ do
            let ([s,y,x],varSet) = mkVars 3
            let dontCare = summer DontCare [x] [y] [s]
            let dontCareAssignments = map (assignmentFromString varSet) [
                    "000",
                    "011",
                    "101",
                    "110"
                    ]
            toTruthTable dontCare `shouldBe` trueOnlyForAssignments varSet dontCareAssignments
            let forbid = summer Forbid [x] [y] [s]
            let forbidAssigments = map (assignmentFromString varSet) [
                    "000",
                    "011",
                    "101"
                    ]
            toTruthTable forbid `shouldBe` trueOnlyForAssignments varSet forbidAssigments

        it "has the correct truth table for 3 bits" $ do
            let ([s0,s1,s2,cOut,y0,y1,y2,x0,x1,x2],varSet) = mkVars 10
            let trueAssignments = map (assignmentFromString varSet) addition3Bit
            let smer = summer (Connect cOut) [x2,x1,x0] [y2,y1,y0] [s2,s1,s0]
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "can forbid overflow" $ do
            let ([s0,s1,y0,y1,x0,x1],varSet) = mkVars 6
            let ([s1',s0'], cOut') = summerSegment Nothing [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Not cOut']
            let smer = summer Forbid [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString varSet) addition2BitForbidOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments varSet trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "can not care about overflow" $ do
            let ([s0,s1,y0,y1,x0,x1],varSet) = mkVars 6
            let ([s1',s0'], _) = summerSegment Nothing [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0']]
            let smer = summer DontCare [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString varSet) addition2BitDontCareOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments varSet trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "summerWithCarry" $ do
        it "uses an incoming carry in 1-bit" $ do
            let [x,y,cIn,cOut,s] = map Atom $ makeVars 5
            let f = summerWithCarry (Just cIn) (Connect cOut) [x] [y] [s]
            f `shouldBe` And [
                Equiv [cOut, Or [And [y,cIn], And [x,cIn], And [x,y]]],
                Equiv [Xor [x,y,cIn], s]
                ]
        it "uses an incoming carry in 2-bit" $ do
            let [x1,x2,y1,y2,cIn,cOut,s1,s2] = map Atom $ makeVars 8
            let f = summerWithCarry (Just cIn) (Connect cOut) [x1,x2] [y1,y2] [s1,s2]
            let carryFromLsb = Or [And [y2,cIn], And [x2,cIn], And [x2,y2]]
            let expected = And [
                    Equiv [cOut, Or [And [y1,carryFromLsb], And [x1,carryFromLsb], And [x1,y1]]],
                    Equiv [Xor [x1,y1,carryFromLsb], s1],
                    Equiv [Xor [x2,y2,cIn], s2]
                    ]
            f `shouldBe` expected

    describe "adderCarry" $ do
        let [x0,x1,x2,x3,x4,x5] = map Atom $ makeVars 6
        it "returns the carry for an adder formula" $ do
            adderCarry 0 2 `shouldBe` And [x0, x2]
            adderCarry 1 2 `shouldBe` Or [And [x3, adderCarry 0 2], And [x1, adderCarry 0 2], And [x1, x3]]
            adderCarry 0 3 `shouldBe` And [x0, x3]
            adderCarry 1 3 `shouldBe` Or [And [x4, adderCarry 0 3], And [x1, adderCarry 0 3], And [x1, x4]]
            adderCarry 2 3 `shouldBe` Or [And [x5, adderCarry 1 3], And [x2, adderCarry 1 3], And [x2, x5]]

    describe "nBitAddition and -Multiplication" $ do
        let test (f,name) = do
                describe name $ do
                    let combinations = [(numBits,mode) | numBits <- [1,2,3,4], mode <- [Forbid,DontCare]]
                    let test numBits mode = do
                            let expectedFormula = getFormula $ multiplicationTableBased mode numBits
                            let actualFormula = nBitMultiplication mode numBits
                            actualFormula `equiv` expectedFormula `shouldBe` True
                    forM_ combinations $ \(numBits,mode) -> do
                        it ("is equivalent to the table based version for " ++ show numBits ++ " bit, " ++ show mode ++ " overflow") $ do
                            test numBits mode
        test (nBitAddition,"nBitAddition")
        test (nBitMultiplication,"nBitMultiplication")

    describe "greaterThan" $ do
        let numBits = [1,2,3,4]
        let ops = [
                (greaterThan,greaterThanTableBased,"greaterThanTableBased"),
                (greaterThanEq,greaterThanEqTableBased,"greaterThanEqTableBased")
                ]
        forM_ ops $ \(op,reference,name) -> do
            forM_ numBits $ \b -> do
                describe name $ do
                    it ("is equivalent to the table based version for " ++ show b ++ " bit") $ do
                        let expectedFormula = getFormula $ reference b
                        let actualFormula = op b
                        actualFormula `equiv` expectedFormula `shouldBe` True

    describe "multiplicationTableBased" $ do
        describe "DontCare overflow mode" $ do
            let test numBits =
                    let f = nBitMultiplication DontCare numBits
                        g = getFormula $ multiplicationTableBased DontCare numBits
                    in toTruthTable f `shouldBe` toTruthTable g
            it "returns an equivalent formula as nBitMultiplication (1 Bit)" $ do
                test 1
            it "returns an equivalent formula as nBitMultiplication (2 Bit)" $ do
                test 2
            it "returns an equivalent formula as nBitMultiplication (3 Bit)" $ do
                test 3
        describe "Forbid overflow mode" $ do
            let test numBits assignments expected =
                    let f = getFormula $ multiplicationTableBased Forbid numBits
                        varSet = variableSet f
                    in forM_ assignments $ \str -> do
                        let a = assignmentFromString varSet str
                        a `isModelOf` f `shouldBe` expected
            it "doesn't allow overflowing calculations" $ do
                test 2 ["101000", "111001", "101101", "111110"] False
            it "allows non-overflowing calculations" $ do
                test 2 [
                    "000000", "000001", "000010", "000011",
                    "000100", "010101", "100110", "110111",
                    "001000", "101001"
                    ] True
            it "is False for wrong calculations" $ do
                test 2 ["010000", "100000", "110000", "000101"] False

    describe "operation2" $ do
        let test op numBits assignments expected =
                let f = getFormula $ operation2 op numBits
                    varSet = variableSet f
                in forM_ assignments $ \str -> do
                    -- "001 010" -> x0 und x4
                    let a = assignmentFromString varSet $ reverse str
                    a `isModelOf` f `shouldBe` expected
        it "creates the correct truth table for 1 bit" $ do
            test (<) 1 ["01"] True
            test (<) 1 ["00", "10", "11"] False
            test (<=) 1 ["00", "01", "11"] True
            test (<=) 1 ["10"] False
        it "creates the correct truth table for less than, 2 bits" $ do
            let f = getFormula $ operation2 (<) 2
            let expected = tableFromString (variableSet f) $ unlines [
                    "0000 0",
                    "0001 0",
                    "0010 0",
                    "0011 0",
                    "0100 1",
                    "0101 0",
                    "0110 0",
                    "0111 0",
                    "1000 1",
                    "1001 1",
                    "1010 0",
                    "1011 0",
                    "1100 1",
                    "1101 1",
                    "1110 1",
                    "1111 0"
                    ]
            toTruthTable f `shouldBe` expected
        it "creates the correct truth table for less than or equal, 2 bits" $ do
            let f = getFormula $ operation2 (<=) 2
            let expected = tableFromString (variableSet f) $ unlines [
                    "0000 1",
                    "0001 0",
                    "0010 0",
                    "0011 0",
                    "0100 1",
                    "0101 1",
                    "0110 0",
                    "0111 0",
                    "1000 1",
                    "1001 1",
                    "1010 1",
                    "1011 0",
                    "1100 1",
                    "1101 1",
                    "1110 1",
                    "1111 1"
                    ]
            toTruthTable f `shouldBe` expected
