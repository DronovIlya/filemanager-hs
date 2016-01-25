{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE PatternSynonyms #-}

module Files.Data where

import Data.Ord ( comparing )
import System.Posix.Files 
import System.Posix.Types
import Control.Exception.Base ( IOException )

-- |Type aliase to differ FilePath and Filename.
type FileName = String

-- |Type representing a file.
data FileEntry a = FileEntry {
    path :: FilePath,              -- ^ parent directory path
    file :: File a                 -- ^ file content
  } deriving (Eq, Show)

-- |Different filesystem subtypes.
data File a =
    Directory {                    -- ^ representing directory
    name :: FileName,              -- ^ directory name
    content :: a                   -- ^ file content
  }
  | RegularFile {                  -- ^ representing any type of file
    name :: FileName,              -- ^ regular file name
    content :: a                   -- ^ file content
  }
  | UnkhownFile {                  -- ^ unsupported filesystem's file
    name :: FileName,              -- ^ unkhown file name
    exception :: IOException       -- ^ exception while parsing file
  } deriving (Show, Eq)

-- |All possible information about file content
data FileInfo = FileInfo {
  time       :: EpochTime,         -- ^ file modification time
  mode       :: FileMode           -- ^ file permission
} deriving (Show, Eq, Ord)


                ----------------------
                -- Pattern matching --
                ----------------------

-- |Matches Regular file
matchFile :: FileEntry FileInfo ->          
             (Bool, FileEntry FileInfo)     
matchFile ff@(FileEntry _ RegularFile {})   = (True, ff)
matchFile ff                                = (False, ff)

-- |Matches Directory
matchDir :: FileEntry FileInfo ->
            (Bool, FileEntry FileInfo)
matchDir ff@(FileEntry _ Directory {})      = (True, ff)
matchDir ff                                 = (False, ff)

-- |Matches invalid filenames. We assume that "." and ".." are invalid
matchInvalidFileName :: FileName -> 
                        (Bool, FileName)
matchInvalidFileName ""                     = (True, "")
matchInvalidFileName "."                    = (True, ".")
matchInvalidFileName ".."                   = (True, "..")
matchInvalidFileName fn                     = (False, fn)

-- |Match special dir "..".
matchUpDir :: FileEntry FileInfo ->
              (Bool, FileEntry FileInfo)
matchUpDir ff@(FileEntry _ (Directory ".." _)) = (True, ff)
matchUpDir ff                                  = (False, ff)

-- |Matches any valid file types
pattern IsDir  f  <- (matchDir   -> (True, f))
pattern IsFile f  <- (matchFile  -> (True, f))

-- |Matches ".." dir
pattern IsUpDir f <- (matchUpDir -> (True, f))

-- |Matches all invalid filenames
pattern IsInvalid <- (fst . matchInvalidFileName . name . file -> True)


                ---------------
                -- Instances --
                ---------------

-- | Compare constructors in order : UnkhownFile, Directory, RegularFile
-- If case of same constructor compare filenames
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

-- |Compare file entries. At first compare names, then compares 'File' constructors
instance Ord (FileEntry FileInfo) where
  compare (FileEntry p a) (FileEntry p' a') = 
    case compare p p' of
      EQ -> compare a a'
      d -> d

                -----------
                -- Utils --
                -----------

-- |Direct compare files.
-- Correct ordering : UnkhownFile, Directory, RegularFile
compareData :: File FileInfo ->
               File FileInfo ->
               Ordering
compareData (UnkhownFile _ _) (Directory _ _)       = LT
compareData (UnkhownFile _ _) (RegularFile _ _)     = LT
compareData (Directory _ _) (UnkhownFile _ _)       = GT
compareData (Directory _ _) (RegularFile _ _)       = LT
compareData (RegularFile _ _) (UnkhownFile _ _)     = GT
compareData (RegularFile _ _) (Directory _ _)       = GT