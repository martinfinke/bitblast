module Main where

import TruthTable
import Formula
import NormalForm
import QuineMcCluskey

[a2,a1,b2,b1,p8,p4,p2,p1] = map (Atom . var) [0..7]

twoBitMultDnf = Or [
    And [Not a2, Not a1, Not b2, Not b1, Not p8, Not p4, Not p2, Not p1],
    And [Not a2, Not a1, Not b2, b1, Not p8, Not p4, Not p2, Not p1],
    And [Not a2, Not a1, b2, Not b1, Not p8, Not p4, Not p2, Not p1],
    And [Not a2, Not a1, b2, b1, Not p8, Not p4, Not p2, Not p1],

    And [Not a2, a1, Not b2, Not b1, Not p8, Not p4, Not p2, Not p1],
    And [Not a2, a1, Not b2, b1, Not p8, Not p4, Not p2, p1],
    And [Not a2, a1, b2, Not b1, Not p8, Not p4, p2, Not p1],
    And [Not a2, a1, b2, b1, Not p8, Not p4, p2, p1],

    And [a2, Not a1, Not b2, Not b1, Not p8, Not p4, Not p2, Not p1],
    And [a2, Not a1, Not b2, b1, Not p8, Not p4, p2, Not p1],
    And [a2, Not a1, b2, Not b1, Not p8, p4, Not p2, Not p1],
    And [a2, Not a1, b2, b1, Not p8, p4, p2, Not p1],

    And [a2, a1, Not b2, Not b1, Not p8, Not p4, Not p2, Not p1],
    And [a2, a1, Not b2, b1, Not p8, Not p4, p2, Not p1],
    And [a2, a1, b2, Not b1, Not p8, p4, p2, Not p1],
    And [a2, a1, b2, b1, p8, Not p4, Not p2, p1]
    ]


main = do
    let optimizedCnf = formulaToPrimesFormula (getFormula $ toCanonicalCnf twoBitMultDnf)
    putStrLn $ show optimizedCnf