{-
  This module exports the functionality of interacting with file
  These module containts following actions :

       * Copy a file.

       * Copy a directory.

       * Copy list of files

       * Copy list of directories

       * Delete a file

       * Delete list of files

       * Open a file

       * Create a file

-}
module Files.Operations where

import Files.Data
import Files.Manager
import Files.Errors
import System.FilePath ( (</>) )
import System.Posix.Files
import System.Posix.IO ( closeFd, createFile)
import Data.Foldable ( for_ )
import Control.Monad
import System.Posix.Types ( FileMode )
import System.Process ( spawnProcess, ProcessHandle )

import qualified System.Directory as D

   --------------------------------
   -- File and Directory copying --
   --------------------------------

-- |Takes a list of files(directory or regular files), and attempts to
-- copy them to a determined location
copyFiles :: [FileEntry FileInfo] ->      -- ^ list of files
             FileEntry FileInfo ->        -- ^ destination path
             IO ()
copyFiles [] _ = return ()
copyFiles fs dest = forM_ fs $ \f -> copy f dest

-- |Copy a single file to a given location
-- Uses pattern matching to determine all valid cases
-- TODO: throws exception on invalid case
copy :: FileEntry FileInfo ->             -- ^ src files
        FileEntry FileInfo ->             -- ^ destination
        IO ()
copy from@(FileEntry _ RegularFile {})    -- ^ copy file to directory
       to@(FileEntry _ Directory {}) = copyFileToDir from to
copy from@(FileEntry _ Directory {})      -- ^ copy directory to directory
       to@(FileEntry _ Directory {}) = copyDirToDir from to
copy _ _ = return ()                      -- ^ unkhown files are not copied

-- |Copy a single file to a given directory
-- TODO: throws exception on invalid case
copyFileToDir :: FileEntry FileInfo ->    -- ^ src file
                 FileEntry FileInfo ->    -- ^ dest directory
                 IO ()
copyFileToDir from@(FileEntry _ (RegularFile fn _))
                to@(FileEntry _ Directory {})
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath   = Files.Manager.getFullPath to </> fn
    copyFile fromPath toPath
copyFileToDir _ _ = return ()              -- throw exception

-- |Recursively coping a directory to a given directory 
-- Use pattern matching to determine invalid cases
-- TODO: throws exception on invalid case
copyDirToDir :: FileEntry FileInfo ->      -- ^ src directory
                FileEntry FileInfo ->      -- ^ dest directory
                IO ()
copyDirToDir IsInvalid _ = return ()       -- throw exception
copyDirToDir _ IsInvalid = return ()       -- throw exception
copyDirToDir from@(FileEntry _ (Directory fn _))
               to@(FileEntry _ Directory {})
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath = Files.Manager.getFullPath to </> fn

    -- check directory not exists, otherwise System.Directory will through exception  
    throwDirExists toPath
    D.createDirectory toPath
    
    toDir <- Files.Manager.readFile toPath
    fromFiles <- Files.Manager.readDirectory fromPath

    -- recursively copying all directory's files
    for_ fromFiles $ \f ->
      case f of
        (FileEntry _ Directory {})   -> copyDirToDir f toDir
        (FileEntry _ RegularFile {}) -> copyFileToDir f toDir
        _                              -> return ()
copyDirToDir _ _ = return ()             -- throw exception

-- |System call to copy file
copyFile :: FilePath ->                  -- ^ src path
            FilePath ->                  -- ^ dest path
            IO()
copyFile = D.copyFile


   ---------------------------------
   -- File and Directory deleting --
   ---------------------------------

-- |Takes a list of files(directory or regular files), and attempts to
-- delete them.
deleteFiles :: [FileEntry FileInfo] ->    -- ^ list of files
               IO ()
deleteFiles [] = return ()
deleteFiles fs = forM_ fs $ \f -> delete f

-- |Delete a single file&
-- Uses pattern matching to determine all valid cases
-- TODO: throws exception on invalid case
delete :: FileEntry FileInfo ->           -- ^ src file
           IO ()
delete f@(FileEntry _ RegularFile {}) = deleteFile f 
delete f@(FileEntry _ Directory {})   = deleteDirectory f
delete _ = return ()                      -- throw exception

-- |Recursively delete whole directory
-- Check invalid directory
-- TODO: throws exception on invalid case
deleteDirectory :: FileEntry FileInfo ->  -- ^ src directory 
                   IO ()
deleteDirectory IsInvalid = return ()     -- throw exception
deleteDirectory f@(FileEntry _ Directory {}) = do
  let fp = Files.Manager.getFullPath f
  files <- Files.Manager.readDirectory fp

  -- recursively delete all files
  for_ files $ \file ->
    case file of
      (FileEntry _ RegularFile {}) -> deleteFile file
      (FileEntry _ Directory {})   -> deleteDirectory file
      _ -> return ()                     -- throw exception
  D.removeDirectory fp
deleteDirectory _ = return ()            -- throw exception

-- |Delete a single file. Use system call
-- TODO: throws exception on invalid case
deleteFile :: FileEntry FileInfo ->      -- ^ src file
              IO ()
deleteFile IsInvalid = return ()         -- throw exception 
deleteFile f@(FileEntry _ RegularFile {}) = D.removeFile (Files.Manager.getFullPath f)
deleteFile _ = return ()                 -- throw exception

   ---------------------------------
   -- Opening and Creating files  --
   ---------------------------------

-- |Open a file in separate process. 
-- DANGEROUS: used Mac Os command "open"
openFile :: FileEntry FileInfo ->        -- ^ opened file path
            IO ProcessHandle
openFile f = spawnProcess "open" [Files.Manager.getFullPath f]

-- |Create a file in filesystem
-- TODO: validate file name
createFile' :: FileEntry FileInfo ->     -- ^ new file path
               String ->                 -- ^ new file name
               IO ()
createFile' ff str = do
  let path = getFullPath ff </> str
  fd <- createFile path newFilePerms
  closeFd fd

-- |FileModes for creating&
newFilePerms :: FileMode
newFilePerms
  =                  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode
