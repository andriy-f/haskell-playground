module GenericShapeSpec where

import GenericShape
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "Circle area tests" $ do
    it "zero radius" $ area (Circle 1 2 0) `shouldBe` 0

    -- expected value is 12.5663706144, but we use approximate value
    it "positive radius" $
      let precision = 0.000000001
          actual = area (Circle 2 2 (2 :: Float))
          expected = 12.5663706144 :: Float
       in abs (actual - expected) < precision `shouldBe` True
