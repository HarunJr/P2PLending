module UtilPrac where

import Prelude

import System.Directory 
import System.FilePath.Posix 

main :: IO ()
main = createAndWriteFile "wallets/borrower/" "Something"

-- createDirectory :: FilePath -> IO ()
-- createDirectory path =

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFile path content