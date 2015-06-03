module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import Variable hiding (eval)
import Control.Monad(forM_)
import MinimizeFormula
import Debug.Trace(traceShow)
import qualified Data.Set as Set
import ArithmeticTruthTables

spec :: Spec
spec = do
    let trueOnlyForAssignments posMapping assignments = foldr (flip setRow True) (allFalseTable posMapping) assignments
    describe "halfAdder" $ do
        it "has the correct truth table" $ do
            let (vars, posMapping) = generateVars 4
            let [c,s,y,x] = map Atom vars
            let ha = halfAdder (x,y) (s,c)
            let trueAssignments = map (assignmentFromString posMapping) [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            toTruthTable ha `shouldBe` trueOnlyForAssignments posMapping trueAssignments

    describe "fullAdderSegment" $ do
        let (vars, posMapping) = generateVars 5
        let [s,cOut,cIn,y,x] = map Atom vars
        let (s',cOut') = fullAdderSegment (x,y) cIn
        let connectedOutputs = And [Equiv [s,s'], Equiv [cOut,cOut']]
        it "has the correct truth table when the outputs are connected" $ do
            let trueAssignments = map (assignmentFromString posMapping) [
                    "00000",
                    "00101",
                    "01001",
                    "01110",
                    "10001",
                    "10110",
                    "11010",
                    "11111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping trueAssignments

        it "is equivalent to a fullAdder when the outputs are connected" $ do
            let fa = fullAdder (x,y) (cIn,cOut) s
            toTruthTable connectedOutputs `shouldBe` toTruthTable fa

    describe "summerSegment" $ do
        it "is equivalent to a fullAdderSegment for 1 bit" $ do
            let (vars, posMapping) = generateVars 2
            let [y,x] = map Atom vars
            let (s', cOut') = halfAdderSegment (x,y)
            let (s'', cOut'') = summerSegment [x] [y]
            let equiv = And [Equiv [s', head s''], Equiv [cOut', cOut'']]
            toTruthTable equiv `shouldBe` allTrueTable posMapping

        it "has the correct truth table for 2 bits" $ do
            let (vars, posMapping) = generateVars 7
            let [cOut,s0,s1,y0,y1,x0,x1] = map Atom vars
            
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Equiv [cOut, cOut']]
            let smer = summer (Connect cOut) [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString posMapping) addition2BitConnectOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments posMapping trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments posMapping trueAssignments

    describe "summer" $ do
        it "has the correct truth table for 1 bit" $ do
            let (vars, posMapping) = generateVars 3
            let [s,y,x] = map Atom vars
            let dontCare = summer DontCare [x] [y] [s]
            let dontCareAssignments = map (assignmentFromString posMapping) [
                    "000",
                    "011",
                    "101",
                    "110"
                    ]
            toTruthTable dontCare `shouldBe` trueOnlyForAssignments posMapping dontCareAssignments
            let forbid = summer Forbid [x] [y] [s]
            let forbidAssigments = map (assignmentFromString posMapping) [
                    "000",
                    "011",
                    "101"
                    ]
            toTruthTable forbid `shouldBe` trueOnlyForAssignments posMapping forbidAssigments
            

        it "has the correct truth table for 3 bits" $ do
            let (vars, posMapping) = generateVars 10
            let [s0,s1,s2,cOut,y0,y1,y2,x0,x1,x2] = map Atom vars
            let trueAssignments = map (assignmentFromString posMapping) addition3Bit
            let smer = summer (Connect cOut) [x2,x1,x0] [y2,y1,y0] [s2,s1,s0]
            toTruthTable smer `shouldBe` trueOnlyForAssignments posMapping trueAssignments

        it "can forbid overflow" $ do
            let (vars, posMapping) = generateVars 6
            let [s0,s1,y0,y1,x0,x1] = map Atom vars
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Not cOut']
            let smer = summer Forbid [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString posMapping) addition2BitForbidOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments posMapping trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments posMapping trueAssignments

        it "can not care about overflow" $ do
            let (vars, posMapping) = generateVars 6
            let [s0,s1,y0,y1,x0,x1] = map Atom vars
            let ([s1',s0'], _) = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0']]
            let smer = summer DontCare [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map (assignmentFromString posMapping) addition2BitDontCareOverflow
            toTruthTable equiv `shouldBe` trueOnlyForAssignments posMapping trueAssignments
            toTruthTable smer `shouldBe` trueOnlyForAssignments posMapping trueAssignments

    describe "multiplierSegmentDontCareOverflow (from Boolector)" $ do
        it "multiplies two one-bit numbers" $ do
            let (vars,posMapping) = generateVars 2
            let [x,y] = map Atom vars
            let mulSeg = multiplierSegmentDontCareOverflow [x] [y]
            let prod = last mulSeg
            let trueAssignments = map (assignmentFromString posMapping) [
                    "11"
                    ]
            toTruthTable prod `shouldBe` trueOnlyForAssignments posMapping trueAssignments
        it "has the correct truth table for 1 bit" $ do
            let (vars,posMapping) = generateVars 3
            let [s,y,x] = map Atom vars
            let mulSeg = multiplierSegmentDontCareOverflow [x] [y]
            let connectedOutputs = Equiv [s, last mulSeg]
            let trueAssignments = map (assignmentFromString posMapping) [
                    "000",
                    "010",
                    "100",
                    "111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping trueAssignments
        it "has the correct truth table for 2 bits" $ do
            let (vars, posMapping) = generateVars 6
            let [s0,s1,y0,y1,x0,x1] = map Atom vars
            let [s1',s0'] = multiplierSegmentDontCareOverflow [x1,x0] [y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1']
                    ]
            let trueAssignments = map (assignmentFromString posMapping) multiplication2BitDontCareOverflow
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping trueAssignments

    describe "multiplierSegment (combinatorial)" $ do
        it "multiplies two one-bit numbers" $ do
            let (vars,posMapping) = generateVars 2
            let [x,y] = map Atom vars
            let mulSeg = multiplierSegment [x] [y]
            let prod = last mulSeg
            let trueAssignments = map (assignmentFromString posMapping) [
                    "11"
                    ]
            toTruthTable prod `shouldBe` trueOnlyForAssignments posMapping trueAssignments
        it "has the correct truth table for 1 bit" $ do
            let (vars,posMapping) = generateVars 3
            let [s,y,x] = map Atom vars
            let mulSeg = multiplierSegment [x] [y]
            let connectedOutputs = Equiv [s, last mulSeg]
            let trueAssignments = map (assignmentFromString posMapping) [
                    "000",
                    "010",
                    "100",
                    "111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping trueAssignments
        it "has the correct truth table for 2 bits" $ do
            let (vars, posMapping) = generateVars 8
            let [s0,s1,s2,s3,y0,y1,x0,x1] = map Atom vars
            let [s3',s2',s1',s0'] = multiplierSegment [x1,x0] [y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2'],
                    Equiv [s3,s3']
                    ]
            let trueAssignments = map (assignmentFromString posMapping) multiplication2BitConnectOverflow
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping trueAssignments
            let generatedTable = map (assignmentFromString posMapping) (multiplicationTableGen 2 4)
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping generatedTable

        it "has the correct truth table for 3 bits" $ do
            let (vars, posMapping) = generateVars 12
            let [s0,s1,s2,s3,s4,s5,y0,y1,y2,x0,x1,x2] = map Atom vars
            let [s5',s4',s3',s2',s1',s0'] = multiplierSegment [x2,x1,x0] [y2,y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2'],
                    Equiv [s3,s3'],
                    Equiv [s4,s4'],
                    Equiv [s5,s5']
                    ]
            let generatedTable = map (assignmentFromString posMapping) (multiplicationTableGen 3 6)
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping generatedTable
        it "has the correct truth table for 4 bits" $ do
            let (vars, posMapping) = generateVars 16
            let [s0,s1,s2,s3,s4,s5,s6,s7,y0,y1,y2,y3,x0,x1,x2,x3] = map Atom vars
            let [s7',s6',s5',s4',s3',s2',s1',s0'] = multiplierSegment [x3,x2,x1,x0] [y3,y2,y1,y0]
            let connectedOutputs = And [
                    Equiv [s0,s0'],
                    Equiv [s1,s1'],
                    Equiv [s2,s2'],
                    Equiv [s3,s3'],
                    Equiv [s4,s4'],
                    Equiv [s5,s5'],
                    Equiv [s6,s6'],
                    Equiv [s7,s7']
                    ]
            let generatedTable = map (assignmentFromString posMapping) (multiplicationTableGen 4 8)
            -- TODO: This takes too long
            --toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments posMapping generatedTable
            pending



(vars, posMapping) = generateVars 8
[s0,s1,s2,s3,y0,y1,x0,x1] = map Atom vars
