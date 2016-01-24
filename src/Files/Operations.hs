module Files.Operations where

import Files.Data
import Files.Manager
import Files.Errors
import System.FilePath
  (
    (</>)
  )

import System.Posix.Files

import System.Posix.IO
  (
    closeFd
  , createFile
  )
import Data.Foldable
  (
    for_
  )
import Control.Monad

import System.Posix.Types
  (
    FileMode
  )
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )

import qualified System.Directory as D

   --------------------------------
   -- File and Directory copying --
   --------------------------------

copyFiles :: [FileEntry FileInfo] ->
             FileEntry FileInfo ->
             IO ()
copyFiles [] _ = return ()
copyFiles fs dest = forM_ fs $ \f -> copy f dest

copy :: FileEntry FileInfo ->
        FileEntry FileInfo ->
        IO ()
copy from@(FileEntry _ RegularFile {})
       to@(FileEntry _ Directory {}) = copyFileToDir from to
copy from@(FileEntry _ Directory {})
       to@(FileEntry _ Directory {}) = copyDirToDir from to
copy _ _ = return ()

copyFileToDir :: FileEntry FileInfo ->
                 FileEntry FileInfo ->
                 IO ()
copyFileToDir from@(FileEntry _ (RegularFile fn _))
                to@(FileEntry _ Directory {})
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath   = Files.Manager.getFullPath to </> fn
    copyFile fromPath toPath
copyFileToDir _ _ = return () -- throw exception

copyDirToDir :: FileEntry FileInfo ->
                FileEntry FileInfo ->
                IO ()
copyDirToDir IsInvalid _ = return () -- throw exception
copyDirToDir _ IsInvalid = return () -- throw exception
copyDirToDir from@(FileEntry _ (Directory fn _))
               to@(FileEntry _ Directory {})
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath = Files.Manager.getFullPath to </> fn

    throwDirExists toPath
    D.createDirectory toPath
    
    toDir <- Files.Manager.readFile toPath
    fromFiles <- Files.Manager.readDirectory fromPath

    for_ fromFiles $ \f ->
      case f of
        (FileEntry _ Directory {})   -> copyDirToDir f toDir
        (FileEntry _ RegularFile {}) -> copyFileToDir f toDir
        _                              -> return ()
copyDirToDir _ _ = return () -- throw exception

copyFile :: FilePath ->
            FilePath ->
            IO()
copyFile = D.copyFile


   --------------------------------
   -- File and Directory deleting --
   --------------------------------

deleteFiles :: [FileEntry FileInfo] ->
               IO ()
deleteFiles [] = return ()
deleteFiles fs = forM_ fs $ \f -> delete f

delete :: FileEntry FileInfo ->
           IO ()
delete f@(FileEntry _ RegularFile {}) = deleteFile f 
delete f@(FileEntry _ Directory {})   = deleteDirectory f
delete _ = return () -- throw exception

deleteDirectory :: FileEntry FileInfo ->
                   IO ()
deleteDirectory IsInvalid = return () -- throw exception
deleteDirectory f@(FileEntry _ Directory {}) = do
  let fp = Files.Manager.getFullPath f
  files <- Files.Manager.readDirectory fp
  for_ files $ \file ->
    case file of
      (FileEntry _ RegularFile {}) -> deleteFile file
      (FileEntry _ Directory {})   -> deleteDirectory file
      _ -> return () -- throw exception
  D.removeDirectory fp
deleteDirectory _ = return () -- throw exception

deleteFile :: FileEntry FileInfo ->
              IO ()
deleteFile IsInvalid = return () -- throw exception 
deleteFile f@(FileEntry _ RegularFile {}) = D.removeFile (Files.Manager.getFullPath f)
deleteFile _ = return () -- throw exception



openFile :: FileEntry FileInfo ->
            IO ProcessHandle
openFile f = spawnProcess "open" [Files.Manager.getFullPath f]