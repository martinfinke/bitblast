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
                    let f = fst $ nBitMultiplication numBits
                        g = getFormula $ multiplication DontCare numBits
                    in toTruthTable f `shouldBe` toTruthTable g
            it "returns an equivalent formula as nBitMultiplication (1 Bit)" $ do
                test 1
            it "returns an equivalent formula as nBitMultiplication (2 Bit)" $ do
                test 2
            it "returns an equivalent formula as nBitMultiplication (3 Bit)" $ do
                test 3
        describe "Forbid overflow mode" $ do
            let test numBits assignments expected =
                    let f = getFormula $ multiplication Forbid numBits
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