module Files.Operations where

import Files.Data
import Files.Manager
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

import System.Posix.Types
  (
    FileMode
  )

import qualified System.Directory as D

   --------------------------------
   -- File and Directory copying --
   --------------------------------

copy :: FileEntry FileInfo ->
        FileEntry FileInfo ->
        IO ()
copy from@(FileEntry _ (RegularFile {}))
       to@(FileEntry _ (Directory {})) = copyFileToDir from to
copy from@(FileEntry _ (Directory {}))
       to@(FileEntry _ (Directory {})) = copyDirToDir from to
copy _ _ = return ()

copyFileToDir :: FileEntry FileInfo ->
                 FileEntry FileInfo ->
                 IO ()
copyFileToDir from@(FileEntry _ (RegularFile fn _))
                to@(FileEntry _ (Directory {}))
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath   = Files.Manager.getFullPath to </> fn
    copyFile fromPath toPath
copyFileToDir _ _ = return () 

copyDirToDir :: FileEntry FileInfo ->
                FileEntry FileInfo ->
                IO ()
copyDirToDir IsInvalid _ = return ()
copyDirToDir _ IsInvalid = return ()
copyDirToDir from@(FileEntry _ (Directory fn _))
               to@(FileEntry _ (Directory {}))
  = do
    let fromPath = Files.Manager.getFullPath from
        toPath = Files.Manager.getFullPath to </> fn
    -- TODO: check if exists
    D.createDirectory toPath
    
    toDir <- Files.Manager.readFile toPath
    fromFiles <- Files.Manager.readDirectory fromPath

    for_ fromFiles $ \f ->
      case f of
        (FileEntry _ (Directory {}))   -> copyDirToDir f toDir
        (FileEntry _ (RegularFile {})) -> copyFileToDir f toDir
        _                              -> return ()
copyDirToDir _ _ = return () 

copyFile :: FilePath ->
            FilePath ->
            IO()
copyFile from to = D.copyFile from to