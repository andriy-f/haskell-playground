module BinaryTreeModuleSpec where

import BinaryTreeModule (BinaryTree (..), singleton, treeInsert)
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
