module Heathrow2LondonSpec where

import Heathrow2London (PathStep (..), RoadType (A, B, C), getHeathrow2LondonMinPathLength, getHeathrow2LondonOptimalPath)
import Test.Hspec

spec :: Spec
spec = do
  describe "getHeathrow2LondonMinPathLength" $ do
    it "standard path" $ do
      getHeathrow2LondonMinPathLength "50,10,30,5,90,20,40,2,25,10,8,0" `shouldBe` 75
    it "tricky 1 path" $ do
      getHeathrow2LondonMinPathLength "1,3,10,1,3,10,1,3,100,200,3,0" `shouldBe` 12

  describe "getHeathrow2LondonOptimalPath" $ do
    it "standard path" $ do
      last (getHeathrow2LondonOptimalPath "50,10,30,5,90,20,40,2,25,10,8,0") `shouldBe` PathStep B 75
    it "tricky 1 path" $ do
      last (getHeathrow2LondonOptimalPath "1,3,10,1,3,10,1,3,100,200,3,0") `shouldBe` PathStep B 12
