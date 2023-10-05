module PlayTypesSpec where

import PlayTypes (AnimalL, AnimalR, MyVector2D, animalName)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Animal" $ do
    -- let an0 = (AnimalL "Beaver" 4 1)
    it "Should compile" $ 0 `shouldBe` 0
    it "Animal assign" $ let anName = "Beaver" in anName `shouldBe` "Beaver"
