module MyByteStrings (myCopyFile) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

getSomeByteString =
  -- hex notation (0x62 = 98, 0x6F = 111)
  -- bsArr is array of Word8, (0-255) or (0x00-0xFF)
  let bsArr = [0x62 .. 0x6F]
   in BSL.pack bsArr

getSomeByteString2 =
  let bsArr = [98 .. 120]
   in BSL.pack bsArr

getSomeByteString3 =
  let bsArr = [40 .. 45]
   in BSL.pack bsArr

bsLazyCons =
  let connected = BSL.cons' 0x00 $ BSL.pack [0x01 .. 0x0F]
  in connected

bsStrictCons =
  let connected = BSL.cons 0x00 $ BSL.pack [0x01 .. 0x0F]
  in connected

-- my implementation of copyFile from System.Directory
myCopyFile :: FilePath -> FilePath -> IO ()
myCopyFile source dest = do
  contents <- BSL.readFile source
  BSL.writeFile dest contents
