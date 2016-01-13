module IO.Utils where

import System.Posix.Files
import System.Posix.User
import System.Directory

obtainDirectory :: FilePath -> IO [FilePath]
obtainDirectory path = do
    contents <- getDirectoryContents path
    return contents

getHomeFolder :: IO FilePath
getHomeFolder = getLoginName >>= (\name -> return ("/Users/" ++ name))
