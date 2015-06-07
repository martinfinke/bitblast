module UtilsSpec where

import SpecHelper
import Utils

spec :: Spec
spec = do
    describe "mapWithRest" $ do
        it "maps each value with all other values" $ do
            let f val others = map (+val) others
            mapWithRest f [0,4,1,9] `shouldBe` [[4,1,9], [4,5,13], [1,5,10], [9,13,10]]