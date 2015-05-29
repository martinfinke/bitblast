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

    describe "summer" $ do
        it "does" $ do
            pending
        --it "is equivalent to a fullAdder for 1 bit" $ do
        --    let [s,cOut,cIn,y,x] = map (Atom . var) [0..4]
        --    let fa = fullAdder (x,y) (cIn,cOut) s
        --    let smer = summer [x] [y] (cIn,cOut) [s]
        --    toTruthTable smer `shouldBe` toTruthTable fa

        --it "has the correct truth table for 2 bits" $ do
        --    let [x1,x0,y1,y0,cIn,cOut,s1,s0] = reverse $ map (Atom . var) [0..7]
        --    let smer = summer [x1,x0] [y1,y0] (cIn,cOut) [s1,s0]
        --    let trueAssignments = map assignmentFromString [
        --            "00000000",
        --            "00001001",
        --            "00010001",
        --            "00011010",
        --            "00100010",
        --            "11010100",
        --            "11011101",
        --            "11100101",
        --            "11101110",
        --            "11110110",
        --            "11111111"
        --            ]
        --    let tt1 = toTruthTable smer 
        --    let tt2 = trueOnlyForAssignments trueAssignments 8
        --    let diff = truthTableDiff tt1 tt2
        --    let expectedFormula = And [
        --            Equiv [cOut, Or [And [y1, Or [And [y0, cIn], And [x0, cIn], And [x0, y0]]],
        --                             And [x1, Or [And [y0, cIn], And [x0, cIn], And [x0, y0]]],
        --                             And [x1, y1]]],
        --            Equiv [s1, Xor [x1, y1, Or [And [y0, cIn], And [x0, cIn], And [x0, y0]]]],
        --            Equiv [s0, Xor [x0, y0, cIn]]
        --            ]
        --    smer `shouldBe` expectedFormula
        --    traceShow diff $ toTruthTable smer `shouldBe` trueOnlyForAssignments trueAssignments 8

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