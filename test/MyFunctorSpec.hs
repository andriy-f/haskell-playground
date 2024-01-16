module MyFunctorSpec where

import qualified Data.Map as Map
import MyFunctor
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "Data.Map myFMap" $ do
    it "some" $ myFMap length (Map.fromList [(1, "O"), (2, "Tw")]) `shouldBe` Map.fromList [(1, 1), (2, 2)]
