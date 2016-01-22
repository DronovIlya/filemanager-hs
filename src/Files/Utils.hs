module Files.Utils where

import qualified Control.Exception as E
import qualified System.IO as I

import System.Posix.Files
import System.Directory
import Control.Monad

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
