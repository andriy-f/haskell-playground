module BinaryTreeModuleSpec where

import BinaryTreeModule (BinaryTree (..), singleton, treeInsert, treeElem)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Insert" $ do
    it "Insert into " $
      let
        --  3
        -- / \
        --1   4
        initialTree = Node 3 (singleton 1) (singleton 4)

        --    3
        --   / \
        --  1   4
        --   \
        --    2
        expectedTree = Node 3 (Node 1 EmptyTree (singleton 2)) (singleton 4)
      in treeInsert 2 initialTree `shouldBe` expectedTree

  describe "treeElem" $ do
    it "Empty tree" $
      treeElem 2 EmptyTree `shouldBe` False
    it "Regular tree, doesn't exist" $
      treeElem 2 (Node 3 (singleton 1) (singleton 4)) `shouldBe` False
    it "Regular tree, exists" $
      treeElem 3 (Node 2 (singleton 1) (singleton 3)) `shouldBe` True
