module PlayTypesSpec where

import PlayTypes (AnimalL, AnimalR, MyVector2D(..), getVector2Dx, getVector2Dy, animalLDefault, animalLName, animalRDefault, animalRName)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "AnimalL" $ do
    it "AnimalL assign" $ let an0 = animalLDefault "Beaver" in animalLName an0 `shouldBe` "Beaver"

  describe "AnimalR" $ do
    it "assign" $ let an0 = animalRDefault "Beaver" in animalRName an0 `shouldBe` "Beaver"

  describe "MyVector2D" $ do
    it "assign and get x" $ let v1 = MyVector2D 1 2 in getVector2Dx v1 `shouldBe` 1
    it "assign and get y" $ let v1 = MyVector2D 3 4 in getVector2Dy v1 `shouldBe` 4
