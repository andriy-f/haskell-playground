module PlayTypesSpec where

import PlayTypes (AnimalL (..), AnimalR (..), MyVector2D (..), animalLDefault, animalLName, animalRDefault, animalRName, getVector2Dx, getVector2Dy)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "AnimalL" $ do
    it "AnimalL assign" $ let an0 = AnimalL "Beaver" 4 1 in animalLName an0 `shouldBe` "Beaver"

  describe "AnimalR" $ do
    it "assign" $ let an0 = AnimalR {name = "Bear", numOfLegs = 4, numOfTails = 1} in animalRName an0 `shouldBe` "Bear"
    it "assign and check prop" $ let an0 = AnimalR {name = "Bear", numOfLegs = 4, numOfTails = 1} in name an0 `shouldBe` "Bear"

  describe "MyVector2D" $ do
    it "assign and get x" $ let v1 = MyVector2D 1 2 in getVector2Dx v1 `shouldBe` 1
    it "assign and get y" $ let v1 = MyVector2D 3 4 in getVector2Dy v1 `shouldBe` 4
