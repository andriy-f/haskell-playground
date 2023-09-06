module NDC2014Spec where

import NDC2014
import Test.Hspec

spec :: Spec
spec = do
  describe "printList" $ do
    -- it "empty array" $ printList [] `shouldBe` ""
    it "single elem array" $ printList [1] `shouldBe` "1"
    it "simple array" $ printList [1,2,3,4,5,6] `shouldBe` "123456"
