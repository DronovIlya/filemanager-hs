module Files.Manager where

import System.Posix.Files
import System.Posix.User
import System.Directory
import Control.Monad
import Files.Utils (makeSize, makePermissions)

data FileInfo = FileInfo {
  name :: String,
  size :: String,
  permissions :: String
}

createFileInfo :: [FilePath] -> IO [FileInfo]
createFileInfo paths = forM paths $ \path -> do
  size <- makeSize path
  permissions <- makePermissions path
  return (FileInfo path size permissions)

obtainDirectory :: FilePath -> IO [FilePath]
obtainDirectory path = do
    contents <- getDirectoryContents path
    return contents

getHomeFolder :: IO FilePath
getHomeFolder = getLoginName >>= (\name -> return ("/Users/" ++ name))
