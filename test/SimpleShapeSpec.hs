module SimpleShapeSpec where

import SimpleShape
import Test.Hspec

spec :: Spec
spec = do
  describe "area" $ do
    it "calculates the area of a circle" $ do
      area (Circle 0 0 1) `shouldBe` pi
      area (Circle 0 0 2) `shouldBe` (4 * pi)
    it "calculates the area of a rectangle" $ do
      area (Rectangle 0 0 2 2) `shouldBe` 4
      area (Rectangle 0 0 3 4) `shouldBe` 12
