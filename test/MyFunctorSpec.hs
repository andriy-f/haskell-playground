module MyFunctorSpec where

import qualified Data.Map as Map
import MyFunctor
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "Data.Map fMap" $ do
    it "some" $ fMap length (Map.fromList [(1, "O"), (2, "Tw")]) `shouldBe` Map.fromList [(1, 1), (2, 2)]
