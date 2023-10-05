module PlayTypesSpec where

import PlayTypes (AnimalL, AnimalR, MyVector2D, animalLDefault, animalLName, animalRDefault, animalRName)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "AnimalL" $ do
    it "AnimalL assign" $ let an0 = animalLDefault "Beaver" in animalLName an0 `shouldBe` "Beaver"

  describe "AnimalR" $ do
    it "assign" $ let an0 = animalRDefault "Beaver" in animalRName an0 `shouldBe` "Beaver"
