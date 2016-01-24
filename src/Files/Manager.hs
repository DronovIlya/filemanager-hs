module Files.Manager where

import System.Posix.User
import Control.Monad

import System.FilePath
  (
    (</>)
  )
import Files.Utils 
  (
	  makeSize, 
	  makePermissions,
	  curDirPath,
	  upDirPath
  )

import Data.List 
  (
    sort,
    isPrefixOf
  )
import Files.Data

import Control.Exception
  (
    handle
  )

import Control.Exception.Base
  (
    IOException
  )

import qualified System.Posix.Files as SPF
import qualified System.Posix.Directory as SPD

      
      ---------------------------------------------------
      -- Manager\s API for reading Files and Directories --
      ----------------------------------------------------


readFile :: (FilePath) ->
            IO (FileEntry FileInfo)
readFile fp = do
  fi <- parseFileInfo fp
  readFile' fi fp

readFile' :: FileInfo ->
             FilePath ->
             IO (FileEntry FileInfo)
readFile' fi fp = do
  let curFile = curDirPath fp
      folder = upDirPath fp
  handleError folder curFile $ do
    st <- SPF.getSymbolicLinkStatus fp
    file <- readFile'' st fi curFile
    return $ FileEntry folder file

readFile'' :: SPF.FileStatus ->
              FileInfo ->
              FilePath ->
              IO (File FileInfo)
readFile'' st fi fp
  | SPF.isDirectory st   = return $ Directory   fp fi
  | SPF.isRegularFile st = return $ RegularFile fp fi
  | otherwise            = return $ UnkhownFile fp (userError "UnkhownFile")

readDirectory :: FilePath ->
                 IO [FileEntry FileInfo]
readDirectory fp = do
  fi <- parseFileInfo fp
  filePaths <- getDirectoryFiles fp
  readDirectory' filePaths fi fp

readDirectory' :: [FilePath] ->
                  FileInfo ->
                  FilePath ->
                  IO [FileEntry FileInfo]
readDirectory' files fi fp = do
  content <- mapM (\p -> readFile' fi $ fp </> p) files
  return $ sort content

getDirectoryFiles :: FilePath ->
                     IO [FilePath]
getDirectoryFiles fp = do
  stream <- SPD.openDirStream fp
  dirs <- getDirectoryFiles' stream []
  SPD.closeDirStream stream
  return dirs

getDirectoryFiles' :: SPD.DirStream ->
                      [FilePath] -> 
                      IO [FilePath]
getDirectoryFiles' stream dirs = do
  dir <- SPD.readDirStream stream
  if (dir == "")
    then return dirs
    else getDirectoryFiles' stream (dir : dirs)	


      -------------------------------------
                    -- Contents helper --
      ----------------------------------------------------

obtainContents :: FileEntry FileInfo ->
                  IO [FileEntry FileInfo]
obtainContents ff = readDirectory $ getFullPath ff

      -------------------
      -- HANDLE ERRORS --
      -------------------

handleError :: FilePath -> -- ^ path to file
               FileName -> -- ^ file's name
               IO (FileEntry a) -> -- ^ parsed file entry
               IO (FileEntry a)    -- ^ resuling unkhown file
handleError fp fn = handle (\e -> return $ FileEntry fp $ UnkhownFile fn e)

-- |Parse all information about file
parseFileInfo :: FilePath -> IO FileInfo
parseFileInfo fp = do
  status <- SPF.getSymbolicLinkStatus fp
  return $ FileInfo
    (SPF.modificationTime status)

      -------------------
      -- Some util methods --
      -------------------
getFullPath :: FileEntry a ->
               FilePath
getFullPath (FileEntry folder file) = folder </> name file

getHomeFolder :: IO FilePath
getHomeFolder = getLoginName >>= (\name -> return ("/Users/" ++ name))

isHidden :: FileEntry FileInfo ->
            Bool
isHidden ff = isHiddenFileName (name $ file ff) 

isHiddenFileName :: String ->
                    Bool
isHiddenFileName ".." = False
isHiddenFileName fp 
  | "." `isPrefixOf` fp = True
  | otherwise            = False