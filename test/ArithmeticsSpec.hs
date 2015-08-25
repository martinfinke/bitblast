module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import NormalForm
import Variable hiding (eval)
import qualified Data.Set as Set
import ArithmeticTruthTables
import Control.Monad

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
            let (s'', cOut'') = summerSegment [x] [y]
            let equiv = And [Equiv [s', head s''], Equiv [cOut', cOut'']]
            toTruthTable equiv `shouldBe` allTrueTable varSet

        it "has the correct truth table for 2 bits" $ do
            let ([cOut,s0,s1,y0,y1,x0,x1],varSet) = mkVars 7
            
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
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
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Not cOut']
            let smer = summer Forbid [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString varSet) addition2BitForbidOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments varSet trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "can not care about overflow" $ do
            let ([s0,s1,y0,y1,x0,x1],varSet) = mkVars 6
            let ([s1',s0'], _) = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0']]
            let smer = summer DontCare [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString varSet) addition2BitDontCareOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments varSet trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "multiplierSegmentDontCareOverflow (from Boolector)" $ do
        it "multiplies two one-bit numbers" $ do
            let ([x,y],varSet) = mkVars 2
            let mulSeg = multiplierSegmentDontCareOverflow [x] [y]
            let prod = last mulSeg
            let trueAssignments = map (assignmentFromString varSet) [
                    "11"
                    ]
            toTruthTable prod `shouldBe` trueOnlyForAssignments varSet trueAssignments
        it "has the correct truth table for 1 bit" $ do
            let ([s,y,x],varSet) = mkVars 3
            let mulSeg = multiplierSegmentDontCareOverflow [x] [y]
            let connectedOutputs = Equiv [s, last mulSeg]
            let trueAssignments = map (assignmentFromString varSet) [
                    "000",
                    "010",
                    "100",
                    "111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments
        it "has the correct truth table for 2 bits" $ do
            let ([s0,s1,y0,y1,x0,x1],varSet) = mkVars 6
            let [s1',s0'] = multiplierSegmentDontCareOverflow [x1,x0] [y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1']
                    ]
            let trueAssignments = map (assignmentFromString varSet) multiplication2BitDontCareOverflow
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "has the correct truth table for 3 bits" $ do
            let ([s0,s1,s2,y0,y1,y2,x0,x1,x2],varSet) = mkVars 9
            let [s2',s1',s0'] = multiplierSegmentDontCareOverflow [x2,x1,x0] [y2,y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2']
                    ]
            let trueAssignments = map (assignmentFromString varSet) $ multiplicationTableGen 3 3
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "has the correct truth table for 4 bits" $ do
            let ([s0,s1,s2,s3,y0,y1,y2,y3,x0,x1,x2,x3],varSet) = mkVars 12
            let [s3',s2',s1',s0'] = multiplierSegmentDontCareOverflow [x3,x2,x1,x0] [y3,y2,y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2'],
                    Equiv [s3,s3']
                    ]
            let trueAssignments = map (assignmentFromString varSet) $ multiplicationTableGen 4 4
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "has the correct truth table for 5 bits" $ do
            let ([s0,s1,s2,s3,s4,y0,y1,y2,y3,y4,x0,x1,x2,x3,x4],varSet) = mkVars 15
            let [s4',s3',s2',s1',s0'] = multiplierSegmentDontCareOverflow [x4,x3,x2,x1,x0] [y4,y3,y2,y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2'],
                    Equiv [s3,s3'],
                    Equiv [s4,s4']
                    ]
            let trueAssignments = map (assignmentFromString varSet) $ multiplicationTableGen 5 5
            pendingWith "Takes a little too long, but has passed before. Uncomment to test again."
            --toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "multiplier" $ do
        it "has the correct truth table for 1 bit" $ do
            let ([s,y,x],varSet) = mkVars 3
            let mul = multiplier [x] [y] [s]
            let trueAssignments = map (assignmentFromString varSet) $ multiplicationTableGen 1 1
            toTruthTable mul `shouldBe` trueOnlyForAssignments varSet trueAssignments

        it "has the correct truth table for 3 bits" $ do
            let ([s0,s1,s2,y0,y1,y2,x0,x1,x2],varSet) = mkVars 9
            let trueAssignments = map (assignmentFromString varSet) $ multiplicationTableGen 3 3
            let mul = multiplier [x2,x1,x0] [y2,y1,y0] [s2,s1,s0]
            toTruthTable mul `shouldBe` trueOnlyForAssignments varSet trueAssignments

    describe "multiplication" $ do
        describe "DontCare overflow mode" $ do
            let test numBits =
                    let f = fst $ nBitMultiplication DontCare numBits
                        g = getFormula . fst $ multiplication DontCare numBits
                    in toTruthTable f `shouldBe` toTruthTable g
            it "returns an equivalent formula as nBitMultiplication (1 Bit)" $ do
                test 1
            it "returns an equivalent formula as nBitMultiplication (2 Bit)" $ do
                test 2
            it "returns an equivalent formula as nBitMultiplication (3 Bit)" $ do
                test 3
        describe "Forbid overflow mode" $ do
            let test numBits assignments expected =
                    let f = getFormula . fst $ multiplication Forbid numBits
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
                let f = getFormula . fst $ operation2 op numBits
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
            let f = getFormula . fst $ operation2 (<) 2
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
            let f = getFormula . fst $ operation2 (<=) 2
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
