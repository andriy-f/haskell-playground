module ModulesSpec where

import Modules (findKey, findKey2, mapMember)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "findKey" $ do
    let phoneBook = [("betty", "555-2938"), ("bonnie", "452-2928"), ("patsy", "493-2928"), ("lucille", "205-2928"), ("wendy", "939-8282"), ("penny", "853-2492")]
    it "Nothing if inexistent key" $ findKey "no-such-thing" phoneBook `shouldBe` Nothing
    it "Value if key exists" $ findKey "patsy" phoneBook `shouldBe` Just "493-2928"

  describe "findKey2" $ do
    let phoneBook = [("betty", "555-2938"), ("bonnie", "452-2928"), ("patsy", "493-2928"), ("lucille", "205-2928"), ("wendy", "939-8282"), ("penny", "853-2492")]
    it "Nothing if inexistent key" $ findKey2 "no-such-thing" phoneBook `shouldBe` Nothing
    it "Value if key exists" $ findKey2 "patsy" phoneBook `shouldBe` Just "493-2928"

  describe "mapMember" $ do
    it "mapMember1" $ mapMember `shouldBe` True
