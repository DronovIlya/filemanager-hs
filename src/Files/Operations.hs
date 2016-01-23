module Files.Operations where

import qualified System.Directory as D

copyFile :: FilePath -> FilePath -> IO ()
copyFile fp1 fp2 = do
  D.copyFile fp1 fp2  
