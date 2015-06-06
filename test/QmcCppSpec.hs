module QmcCppSpec where

import SpecHelper
import QmcCpp

import Qm(QmTerm(..))

import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "qmcCppComputePrimes" $ do
        it "works for an empty list of ones" $ do
            qmcCppComputePrimes [] `shouldBe` []

        it "behaves as the python version" $ do
            let test cubes expected = Set.fromList (qmcCppComputePrimes cubes) `shouldBe` Set.fromList (map QmTerm expected)
            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]
            test [3] [(3, 0)]
            test [0,1] [(0, 1)]
            test [1,2] [(2, 0), (1, 0)]
            test [2,3] [(2, 1)]
            test [3,4] [(3, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4] [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] [(2, 4), (2, 1)]
            test [3,4,8] [(3, 0), (8, 0), (4, 0)]

            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]
            test [3] [(3, 0)]
            test [0,1] [(0, 1)]
            test [1,2] [(2, 0), (1, 0)]
            test [2,3] [(2, 1)]
            test [3,4] [(3, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4] [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] [(2, 4), (2, 1)]
            test [3,4,8] [(3, 0), (8, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2)]
            test [1,2,4,9] [(2, 0), (1, 8), (4, 0)]
            test [2,3,6,13] [(2, 1), (2, 4), (13, 0)]
            test [3,4,8,12] [(3, 0), (8, 4), (4, 8)]