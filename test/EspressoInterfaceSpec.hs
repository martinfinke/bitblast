module EspressoInterfaceSpec where

import SpecHelper
import EspressoInterface
import Qm(fromString, getTerm)

spec :: Spec
spec = do
    describe "toPLA" $ do
        it "creates a single-line file for one QmTerm" $ do
            toPLA 3 [fromString "010"] `shouldBe` PLA (unlines [".i 3", ".o 1", "010 1", ".e"])
        it "creates multiple lines for multiple ones" $ do
            toPLA 2 (map fromString ["00", "01", "11"]) `shouldBe` PLA (unlines [".i 2", ".o 1", "00 1", "01 1", "11 1", ".e"])