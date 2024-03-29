module LibSpec where

import           Lib
import           Test.Hspec
import Lib (curryTest)

spec :: Spec
spec = do
    describe "qsort" $ do
        it "empty array" $ qsort [] `shouldBe` ([] :: [Int])
        it "single-item array" $ qsort [2] `shouldBe` ([2] :: [Int])
        it "regular unsorted array"
            $          qsort [2, 7, 1, 4]
            `shouldBe` ([1, 2, 4, 7] :: [Int])
        it "regular sorted array"
            $          qsort [1, 2, 3, 4]
            `shouldBe` ([1, 2, 3, 4] :: [Int])
        it "regular array with duplicates"
            $          qsort [10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9]
            `shouldBe` ([1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9, 10] :: [Int])

    describe "myAdd" $ do
        it "myAdd 1 + 1" $ myAdd 1 1 `shouldBe` (2 :: Int)
        it "myAdd 223 + 486" $ myAdd 223 486 `shouldBe` (709 :: Int)

    describe "add1" $ do
        it "add1 2" $ add1 2 `shouldBe` (3 :: Int)
        it "add1 5" $ add1 5 `shouldBe` (6 :: Int)

    describe "muln" $ do
        it "take 5 $ muln 2 [1..] == 2,4,6,8,10" $ (take 5 $ muln 2 [1..]) `shouldBe` ([2,4,6,8,10] :: [Int])

    describe "pairs" $ do
        it "pairs (+) [1]" $ pairs (+) [1] `shouldBe` [1]
        it "pairs (+) [1,2]" $ pairs (+) [1, 2] `shouldBe` [1, 2]
        it "pairs (+) [1,2,3]" $ pairs (+) [1, 2, 3] `shouldBe` [1, 2, 3]
        it "pairs (+) [1,2,3,4]" $ pairs (+) [1,2,3,4] `shouldBe` [1,2,3,4]

    describe "curryTest" $ do
        it "curryTest 'a' 1" $ curryTest 'a' 1 `shouldBe` "a1"
