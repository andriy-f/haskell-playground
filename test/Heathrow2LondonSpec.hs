module Heathrow2LondonSpec where

import Heathrow2London (getHeathrow2LondonMinPathLength)
import Test.Hspec

spec :: Spec
spec = do
  describe "getHeathrow2LondonMinPathLength" $ do
    it "getHeathrow2LondonMinPathLength" $ do
      getHeathrow2LondonMinPathLength "50,10,30,5,90,20,40,2,25,10,8,0" `shouldBe` 75
