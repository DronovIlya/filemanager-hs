module Files.Manager where

import qualified Control.Exception as E
import qualified System.IO as I

import System.Posix.Files
import System.Posix.User
import System.Directory
import Control.Monad

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

makeSize :: FilePath -> IO String
makeSize path = E.catch (makeSize' path) handler
  where
    handler :: E.IOException -> IO String
    handler _ = return $ show 0

makeSize' :: FilePath -> IO String
makeSize' path = ((I.openFile path I.ReadMode) >>= (\handle -> do
  size <- I.hFileSize handle
  I.hClose handle
  return $ show size ))

makePermissions :: FilePath -> IO String
makePermissions path = getPermissions path >>=
 ( \p -> return ( ( if ( readable p ) then "r" else "-" ) 
                    ++ ( if ( writable p ) then "w" else "-" )
                    ++ ( if ( executable p ) then "x" else  "-" )
                    ++ ( if ( searchable p ) then "s" else "-" ) ) )

obtainDirectory :: FilePath -> IO [FilePath]
obtainDirectory path = do
    contents <- getDirectoryContents path
    return contents

getHomeFolder :: IO FilePath
getHomeFolder = getLoginName >>= (\name -> return ("/Users/" ++ name))
