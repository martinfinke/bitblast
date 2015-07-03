module TruthBased2Spec where

import Variable
import TruthBased2
import SpecHelper

spec :: Spec
spec = do
    describe "expansions" $ do
        it "is the original assignment if no extra variables are allowed" $ do
            pending