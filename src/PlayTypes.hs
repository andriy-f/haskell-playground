module PlayTypes
  ( MyVector2D,
    AnimalL,
    animalName,
    AnimalR,
    MyEqVal,
  )
where

data MyVector2D = MyVector2D Int Int

data AnimalL = AnimalL String Int Int deriving (Show)

animalName (AnimalL name _ _) = name

data AnimalR = AnimalR
  { name :: String,
    numOfLegs :: Int,
    numOfTails :: Int
  }
  deriving (Show)

data MyEqVal = True | False
