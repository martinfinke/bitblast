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
            let summerSeg@([s1',s0'], cOut') = summerSegment [x1,x0] [y1,y0]
            let equiv = And [Equiv [s1, s1'], Equiv [s0, s0'], Equiv [cOut, cOut']]
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
            traceShow summerSeg $ toTruthTable equiv `shouldBe` trueOnlyForAssignments trueAssignments 7

        --it "has the correct truth table for 4 bits" $ do
            -- http://www.pradipyadav.com/2012/08/objective-to-design-and-implement-of-4.html
            --let [a4,a3,a2,a1,b4,b3,b2,b1,c,s4,s3,s2,s1] = map (Atom . var) [0..12]
            --let trueAssignments = map assignmentFromString [
            --        "1000001001010",
            --        "1000100010000",
            --        "0010100001010",
            --        "0001011101000",
            --        "1010101110010",
            --        "1110111111010",
            --        "1010110110111"
            --        ]

            --let smer = summer [a4,a3,a2,a1] [b4,b3,b2,b1] (cIn,cOut) [s]
            --toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 13
            --pending