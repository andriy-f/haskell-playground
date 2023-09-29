module BasicsSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Basics (mul2)

spec :: Spec
spec = do
    describe "mul2 tests" $ do
        it "zero mul2" $ mul2 0 `shouldBe` (0 :: Int)
        it "one mul2" $ mul2 1 `shouldBe` (2 :: Int)
