{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- Expectations
--      Pure:           sqrt 4 `shouldBe` 2
--      IO:             readFile "Hello.txt" `shouldReturn` "Hello World!"
--      List Predicate: evenNumbers `shouldSatisfy` (not . odd)
--      Exceptions:     launchMissiles `shouldThrow` anyException (gibt auch spezifischere)
--      Error-Text:     (evaluate . force) (1 `div` 0) `shouldThrow` errorCall "Division by zero"
--      QuickCheck:     it "is inverse to show" $ property $ \x -> (read . show) x == (x :: Int)
-- Multithreading: Vor dem do zusätzlich parallel schreiben. ghc-options mit "-threaded" und beim Ausführen "cabal test +RTS -N -RTS a-add"


