{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Files.Errors where

import Data.Typeable
import Control.Exception
import Control.Monad
  (
    when
  )

import System.IO.Error
  (
    catchIOError
  )

import qualified System.Posix.Files as SPF

data MyException = DirExists String
  deriving (Show, Typeable)

instance Exception MyException

throwDirExists :: FilePath ->
                  IO ()
throwDirExists fp = do
  exists <- dirExists fp
  when exists $ throw $ DirExists fp

dirExists :: FilePath ->
             IO Bool
dirExists fp =
  catchIOError (dirExists' fp) (\_ -> return False)

dirExists' :: FilePath ->
              IO Bool
dirExists' fp = do
  fs <- SPF.getFileStatus fp
  return $ SPF.isDirectory fs