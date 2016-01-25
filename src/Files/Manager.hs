{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module provides functionality for working with the file system
module Files.Manager where

import System.Posix.User
import Control.Monad

import System.FilePath ( (</>) )
import Files.Utils ( makeTime, curDirPath, upDirPath )

import Data.List ( sort, isPrefixOf )
import Files.Data

import Control.Exception ( handle )

import Control.Exception.Base ( IOException)

import qualified System.Posix.Files as SPF
import qualified System.Posix.Directory as SPD

      
      ---------------------------------------------------
      -- Manager's API for reading Files and Directories --
      ----------------------------------------------------

-- |Read a file into an 'FileEntry' 
readFile :: FilePath ->               -- ^ original file path
            IO (FileEntry FileInfo)
readFile fp = do
  fi <- parseFileInfo fp
  readFile' fi fp

-- |Handle errors while reading file
readFile' :: FileInfo ->              -- ^ file's information
             FilePath ->              -- ^ file's path
             IO (FileEntry FileInfo)
readFile' fi fp = do
  let curFile = curDirPath fp
      folder = upDirPath fp
  handleError folder curFile $ do
    st <- SPF.getSymbolicLinkStatus fp
    file <- readFile'' st fi curFile
    return $ FileEntry folder file

-- |Determine type of file according to POSIX status
readFile'' :: SPF.FileStatus ->
              FileInfo ->
              FilePath ->
              IO (File FileInfo)
readFile'' st fi fp
  | SPF.isDirectory st   = return $ Directory   fp fi
  | SPF.isRegularFile st = return $ RegularFile fp fi
  | otherwise            = return $ UnkhownFile fp (userError "UnkhownFile")

-- |Build a list of FileEntry appropriate to given filepath
-- Include "." and ".."
readDirectory :: FilePath ->              -- ^ directory path
                 IO [FileEntry FileInfo]
readDirectory fp = do
  fi <- parseFileInfo fp              
  filePaths <- getDirectoryFiles fp
  readDirectory' filePaths fi fp

-- |Build a list of FileEntry form list of FilePaths.
-- Sort resulting list
readDirectory' :: [FilePath] ->           -- ^ list of filepath
                  FileInfo ->             -- ^ directory information
                  FilePath ->             -- ^ directory path
                  IO [FileEntry FileInfo]
readDirectory' files fi fp = do
  content <- mapM (\p -> do
    let path = fp </> p
    ffi <- parseFileInfo path
    readFile' ffi path) files
  return $ sort content

-- |Build a list of FilePaths appropriate to given filepath
-- TODO: handle exception while reading
getDirectoryFiles :: FilePath ->          -- ^ directory filepath
                     IO [FilePath]
getDirectoryFiles fp = do
  stream <- SPD.openDirStream fp
  dirs <- getDirectoryFiles' stream []
  SPD.closeDirStream stream
  return dirs

-- |Recursively read all files from directory stream
getDirectoryFiles' :: SPD.DirStream ->    -- ^ directory stream
                      [FilePath] ->       -- ^ current list of file paths
                      IO [FilePath]
getDirectoryFiles' stream dirs = do
  dir <- SPD.readDirStream stream
  if dir == ""
    then return dirs
    else getDirectoryFiles' stream (dir : dirs)


      ---------------------
      -- Contents helper --
      ---------------------

-- |Obtain whle directory by given path
obtainContents :: FileEntry FileInfo ->   -- ^ directory
                  IO [FileEntry FileInfo]
obtainContents ff = readDirectory $ getFullPath ff

      -------------------
      -- Handle errors --
      -------------------

handleError :: FilePath ->                -- ^ path to file
               FileName ->                -- ^ file's name
               IO (FileEntry a) ->        -- ^ parsed file entry
               IO (FileEntry a)           -- ^ resuling unkhown file
handleError fp fn = handle (\e -> return $ FileEntry fp (UnkhownFile fn e))

-- |Parse all information about file.
-- Uses POSIX file status to determine state of file
parseFileInfo :: FilePath ->              -- ^ file path
                 IO FileInfo
parseFileInfo fp = do
  status <- SPF.getSymbolicLinkStatus fp
  return $ FileInfo
    (SPF.statusChangeTime status) 
    (SPF.fileMode status)


      -----------
      -- Utils --
      -----------

-- |Build full path of 'FileEntry' from "/
getFullPath :: FileEntry a ->              -- ^ given file
               FilePath
getFullPath (FileEntry folder file) = folder </> name file

-- |Return home folder.
-- DANGEROUS: Used only on Mac OS
getHomeFolder :: IO FilePath
getHomeFolder = getLoginName >>= (\name -> return ("/Users/" ++ name))

-- |Check hidden file by path. Assume ".." not hidden, kind of up directory
isHiddenFileName :: String ->              -- ^ given path
                    Bool
isHiddenFileName ".."    = False 
isHiddenFileName fp 
  | "." `isPrefixOf` fp  = True
  | otherwise            = False

-- |Check hidden file in appropriate FileEntry
isHidden :: FileEntry FileInfo ->
            Bool
isHidden ff = isHiddenFileName (name $ file ff) 