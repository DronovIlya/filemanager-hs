{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE PatternSynonyms #-}

module Files.Data where

import Control.Applicative
  (
    (<*>)
  , (<$>)
  , (<|>)
  , pure
  )

import Data.Ord
  (
    comparing
  )

import System.Posix.Types 
  (
    EpochTime
  )

import Control.Exception.Base
  (
    IOException
  )
  
type FileName = String

data FileEntry a = FileEntry {
    path :: FilePath,        -- ^ parent directory path
    file :: File a           -- ^ file content
  } deriving (Eq, Show)

data File a =
    Directory {
    name :: FileName,        -- ^ directory name
    content :: a                -- ^ file content
  }
  | RegularFile {
    name :: FileName,        -- ^ regular file name
    content :: a                -- ^ file content
  }
  | UnkhownFile {
    name :: FileName,        -- ^ unkhown file name
    exception :: IOException -- ^ exception while parsing file
  } deriving (Show, Eq)

data FileInfo = FileInfo {
  time       :: String -- ^ file modification time
} deriving (Show, Eq, Ord)

  ----------------------
  -- Pattern matching --
  ----------------------

matchFile :: FileEntry FileInfo ->
             (Bool, FileEntry FileInfo)
matchFile ff@(FileEntry _ RegularFile {}) = (True, ff)
matchFile ff                                = (False, ff)

matchDir :: FileEntry FileInfo ->
            (Bool, FileEntry FileInfo)
matchDir ff@(FileEntry _ Directory {})    = (True, ff)
matchDir ff                                 = (False, ff)

matchInvalidFileName :: FileName -> (Bool, FileName)
matchInvalidFileName ""   = (True, "")
matchInvalidFileName "."  = (True, ".")
matchInvalidFileName ".." = (True, "..")
matchInvalidFileName fn   = (False, fn)

matchUpDir :: FileEntry FileInfo ->
              (Bool, FileEntry FileInfo)
matchUpDir ff@(FileEntry _ (Directory ".." _)) = (True, ff)
matchUpDir ff                                = (False, ff)

pattern IsDir f  <- (matchDir   -> (True, f))
pattern IsFile f <- (matchFile  -> (True, f))

pattern IsUpDir f <- (matchUpDir -> (True, f))

pattern IsInvalid <- (fst . matchInvalidFileName . name . file -> True)


instance Ord (File FileInfo) where
  compare (RegularFile p a) (RegularFile p' a') =
    case compare p p' of
      EQ -> compare a a'
      d -> d
  compare (Directory p a) (Directory p' a') =
    case compare p p' of
      EQ -> compare a a'
      d -> d
  compare p p' = compareData p p'

instance Ord (FileEntry FileInfo) where
  compare (FileEntry p a) (FileEntry p' a') = 
    case compare p p' of
      EQ -> compare a a'
      d -> d

compareData :: File FileInfo ->
               File FileInfo ->
               Ordering
compareData (UnkhownFile _ _) (Directory _ _)  = LT
compareData (UnkhownFile _ _) (RegularFile _ _) = LT
compareData (RegularFile _ _) (UnkhownFile _ _) = GT
compareData (RegularFile _ _) (Directory _ _)       = GT
compareData (Directory _ _) (UnkhownFile _ _)  = GT
compareData (Directory _ _) (RegularFile _ _)       = LT
