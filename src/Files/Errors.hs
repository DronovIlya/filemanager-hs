{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |Provides simple error handling

module Files.Errors where

import Data.Typeable
import Control.Exception
import Control.Monad ( when )
import System.IO.Error ( catchIOError )

import qualified System.Posix.Files as SPF

-- |Declared exceptions
data MyException = DirExists String
  deriving (Show, Typeable)

instance Exception MyException

-- |Throws an exception if the dir exists
throwDirExists :: FilePath ->         -- ^ dir filepath
                  IO ()
throwDirExists fp = do
  exists <- dirExists fp
  when exists $ throw $ DirExists fp

-- |Catch error on checking dir existance
dirExists :: FilePath ->              -- ^ dir filepath
             IO Bool
dirExists fp =
  catchIOError (dirExists' fp) (\_ -> return False)

-- |Check file's appropriate status
dirExists' :: FilePath ->             -- ^ dir filepath
              IO Bool
dirExists' fp = do
  fs <- SPF.getFileStatus fp
  return $ SPF.isDirectory fs