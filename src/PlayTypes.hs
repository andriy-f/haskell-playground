module PlayTypes
  ( MyVector2D (..),
    Vector3D (..),
    vPlus,
    AnimalL,
    animalLName,
    animalLDefault,
    AnimalR,
    animalRDefault,
    animalRName,
    MyEqVal,
  )
where

data MyVector2D = MyVector2D Int Int deriving (Show)

-- Can be Int, Integer, Double (change name)
data Vector3D a = Vector3D a a a deriving (Show)

vPlus :: (Num a) => Vector3D a -> Vector3D a -> Vector3D a
(Vector3D x1 y1 z1) `vPlus` (Vector3D x2 y2 z2) = Vector3D (x1+x2) (y1+y2) (z1+z2)
-- vPlus (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)

data AnimalL = AnimalL String Int Int deriving (Show)

animalLName :: AnimalL -> String
animalLName (AnimalL name _ _) = name

data AnimalR = AnimalR
  { name :: String,
    numOfLegs :: Int,
    numOfTails :: Int
  }
  deriving (Show)

data MyEqVal = True | False

data Suit = Club | Diamond | Heart | Spade
  deriving (Read, Show, Enum, Eq, Ord)

data CardValue
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Read, Show, Enum, Eq, Ord)

animalLDefault :: String -> AnimalL
animalLDefault name = AnimalL name 4 1

animalRDefault :: String -> AnimalR
animalRDefault nameVal = AnimalR {name = nameVal, numOfLegs = 4, numOfTails = 1}

animalRName :: AnimalR -> String
animalRName = name
