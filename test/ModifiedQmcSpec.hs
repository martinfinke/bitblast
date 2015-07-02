module ModifiedQmcSpec where

import SpecHelper
import ModifiedQmc
import QmcTypes
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "modifiedQmc" $ do
        it "behaves as the python version if there are no may's (only must's)" $ do
            let test musts expected = Set.fromList (modifiedQmc musts []) `shouldBe` Set.fromList (map QmTerm expected)

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

        it "keeps the original terms when merging may's" $ do
            let test mays expected = Set.fromList (modifiedQmc [] mays) `shouldBe` Set.fromList (map QmTerm expected)

            test [] []
            test [0] [(0, 0)]
            test [1] [(1, 0)]
            test [2] [(2, 0)]
            test [3] [(3, 0)]
            test [0,1] [(0, 0), (1, 0), (0, 1)]
            test [1,2] [(2, 0), (1, 0)]
            test [2,3] [(2, 0), (3, 0), (2, 1)]
            test [3,4] [(3, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2), (0, 0), (1, 0), (3, 0)]
            test [1,2,4] [(2, 0), (1, 0), (4, 0)]
            test [2,3,6] [(2, 4), (2, 1), (2, 0), (3, 0), (6, 0)]
            test [3,4,8] [(3, 0), (8, 0), (4, 0)]
            test [0,1,3] [(0, 1), (1, 2), (0, 0), (1, 0), (3, 0)]
            test [1,2,4,9] [(2, 0), (1, 8), (4, 0), (1, 0), (9, 0)]
            test [2,3,6,13] [(2, 1), (2, 4), (13, 0), (2, 0), (3, 0), (6, 0)]
            test [3,4,8,12] [(3, 0), (8, 4), (4, 8), (4, 0), (8, 0), (12, 0)]
