module ArithmeticsSpec where

import SpecHelper
import Arithmetics
import Formula
import TruthTable
import Control.Monad(forM_)
import MinimizeFormula
import Debug.Trace(traceShow)

spec :: Spec
spec = do
    let trueOnlyForAssignments assignments numVariables = setOutputs (zip assignments $ repeat (Just True)) (allFalseTable numVariables)
    describe "halfAdder" $ do
        it "has the correct truth table" $ do
            let [c,s,y,x] = map (Atom . var) [0..3]
            let ha = halfAdder (x,y) (s,c)
            let trueAssignments = map assignmentFromString [
                    "0000",
                    "0110",
                    "1010",
                    "1101"
                    ]
            toTruthTable ha `shouldBe` trueOnlyForAssignments trueAssignments 4

    describe "fullAdderSegment" $ do
        let [s,cOut,cIn,y,x] = map (Atom . var) [0..4]
        let (s',cOut') = fullAdderSegment (x,y) cIn
        let connectedOutputs = And [Equiv [s,s'], Equiv [cOut,cOut']]
        it "has the correct truth table when the outputs are connected" $ do
            let trueAssignments = map assignmentFromString [
                    "00000",
                    "00101",
                    "01001",
                    "01110",
                    "10001",
                    "10110",
                    "11010",
                    "11111"
                    ]
            toTruthTable connectedOutputs `shouldBe` trueOnlyForAssignments trueAssignments 5

        it "is equivalent to a fullAdder when the outputs are connected" $ do
            let fa = fullAdder (x,y) (cIn,cOut) s
            toTruthTable connectedOutputs `shouldBe` toTruthTable fa

    describe "summerSegment" $ do
        it "is equivalent to a fullAdderSegment for 1 bit" $ do
            let [y,x] = map (Atom . var) [0,1]
            let (s', cOut') = halfAdderSegment (x,y)
            let (s'', cOut'') = summerSegment [x] [y]
            let equiv = And [Equiv [s', head s''], Equiv [cOut', cOut'']]
            toTruthTable equiv `shouldBe` allTrueTable 2

        it "has the correct truth table for 2 bits" $ do
            let [x1,x0,y1,y0,s1,s0,cOut] = reverse $ map (Atom . var) [0..6]
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Equiv [cOut, cOut']]
            let smer = summer (Connect cOut) [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map assignmentFromString [
                    "0000000",
                    "0001010",
                    "0010100",
                    "0011110",
                    "0100010",
                    "0101100",
                    "0110110",
                    "0111001",
                    "1000100",
                    "1001110",
                    "1010001",
                    "1011011",
                    "1100110",
                    "1101001",
                    "1110011",
                    "1111101"
                    ]
            toTruthTable equiv `shouldBe` trueOnlyForAssignments trueAssignments 7
            toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 7

    describe "summer" $ do
        it "has the correct truth table for 1 bit" $ do
            let [x,y,s] = reverse $ map (Atom . var) [0..2]
            let dontCare = summer DontCare [x] [y] [s]
            let dontCareAssignments = map assignmentFromString [
                    "000",
                    "011",
                    "101",
                    "110"
                    ]
            toTruthTable dontCare `shouldBe` trueOnlyForAssignments dontCareAssignments 3
            let forbid = summer Forbid [x] [y] [s]
            let forbidAssigments = map assignmentFromString [
                    "000",
                    "011",
                    "101"
                    ]
            toTruthTable forbid `shouldBe` trueOnlyForAssignments forbidAssigments 3
            

        it "has the correct truth table for 3 bits" $ do
            let [x2,x1,x0,y2,y1,y0,cOut,s2,s1,s0] = reverse $ map (Atom . var) [0..9]
            let trueAssignments = map assignmentFromString [
                    "000" ++ "000" ++ "0" ++ "000",
                    "000" ++ "001" ++ "0" ++ "001",
                    "000" ++ "010" ++ "0" ++ "010",
                    "000" ++ "011" ++ "0" ++ "011",
                    "000" ++ "100" ++ "0" ++ "100",
                    "000" ++ "101" ++ "0" ++ "101",
                    "000" ++ "110" ++ "0" ++ "110",
                    "000" ++ "111" ++ "0" ++ "111",

                    "001" ++ "000" ++ "0" ++ "001",
                    "001" ++ "001" ++ "0" ++ "010",
                    "001" ++ "010" ++ "0" ++ "011",
                    "001" ++ "011" ++ "0" ++ "100",
                    "001" ++ "100" ++ "0" ++ "101",
                    "001" ++ "101" ++ "0" ++ "110",
                    "001" ++ "110" ++ "0" ++ "111",
                    "001" ++ "111" ++ "1" ++ "000",

                    "010" ++ "000" ++ "0" ++ "010",
                    "010" ++ "001" ++ "0" ++ "011",
                    "010" ++ "010" ++ "0" ++ "100",
                    "010" ++ "011" ++ "0" ++ "101",
                    "010" ++ "100" ++ "0" ++ "110",
                    "010" ++ "101" ++ "0" ++ "111",
                    "010" ++ "110" ++ "1" ++ "000",
                    "010" ++ "111" ++ "1" ++ "001",

                    "011" ++ "000" ++ "0" ++ "011",
                    "011" ++ "001" ++ "0" ++ "100",
                    "011" ++ "010" ++ "0" ++ "101",
                    "011" ++ "011" ++ "0" ++ "110",
                    "011" ++ "100" ++ "0" ++ "111",
                    "011" ++ "101" ++ "1" ++ "000",
                    "011" ++ "110" ++ "1" ++ "001",
                    "011" ++ "111" ++ "1" ++ "010",

                    "100" ++ "000" ++ "0" ++ "100",
                    "100" ++ "001" ++ "0" ++ "101",
                    "100" ++ "010" ++ "0" ++ "110",
                    "100" ++ "011" ++ "0" ++ "111",
                    "100" ++ "100" ++ "1" ++ "000",
                    "100" ++ "101" ++ "1" ++ "001",
                    "100" ++ "110" ++ "1" ++ "010",
                    "100" ++ "111" ++ "1" ++ "011",

                    "101" ++ "000" ++ "0" ++ "101",
                    "101" ++ "001" ++ "0" ++ "110",
                    "101" ++ "010" ++ "0" ++ "111",
                    "101" ++ "011" ++ "1" ++ "000",
                    "101" ++ "100" ++ "1" ++ "001",
                    "101" ++ "101" ++ "1" ++ "010",
                    "101" ++ "110" ++ "1" ++ "011",
                    "101" ++ "111" ++ "1" ++ "100",

                    "110" ++ "000" ++ "0" ++ "110",
                    "110" ++ "001" ++ "0" ++ "111",
                    "110" ++ "010" ++ "1" ++ "000",
                    "110" ++ "011" ++ "1" ++ "001",
                    "110" ++ "100" ++ "1" ++ "010",
                    "110" ++ "101" ++ "1" ++ "011",
                    "110" ++ "110" ++ "1" ++ "100",
                    "110" ++ "111" ++ "1" ++ "101",

                    "111" ++ "000" ++ "0" ++ "111",
                    "111" ++ "001" ++ "1" ++ "000",
                    "111" ++ "010" ++ "1" ++ "001",
                    "111" ++ "011" ++ "1" ++ "010",
                    "111" ++ "100" ++ "1" ++ "011",
                    "111" ++ "101" ++ "1" ++ "100",
                    "111" ++ "110" ++ "1" ++ "101",
                    "111" ++ "111" ++ "1" ++ "110"
                    ]
            let smer = summer (Connect cOut) [x2,x1,x0] [y2,y1,y0] [s2,s1,s0]
            toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 10

        it "can forbid overflow" $ do
            let [x1,x0,y1,y0,s1,s0] = reverse $ map (Atom . var) [0..5]
            let ([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Not cOut']
            let smer = summer Forbid [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map assignmentFromString [
                    "000000",
                    "000101",
                    "001010",
                    "001111",
                    "010001",
                    "010110",
                    "011011",
                    "100010",
                    "100111",
                    "110011"
                    ]
            toTruthTable equiv `shouldBe` trueOnlyForAssignments trueAssignments 6
            toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 6

        it "can not care about overflow" $ do
            let [x1,x0,y1,y0,s1,s0] = reverse $ map (Atom . var) [0..5]
            let ([s1',s0'], _) = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0']]
            let smer = summer DontCare [x1,x0] [y1,y0] [s1,s0]
            let trueAssignments = map assignmentFromString [
                    "000000",
                    "000101",
                    "001010",
                    "001111",
                    "010001",
                    "010110",
                    "011011",
                    "011100",
                    "100010",
                    "100111",
                    "101000",
                    "101101",
                    "110011",
                    "110100",
                    "111001",
                    "111110"
                    ]
            toTruthTable equiv `shouldBe` trueOnlyForAssignments trueAssignments 6
            toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 6