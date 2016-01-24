module Files.Utils where

import qualified Control.Exception as E
import qualified System.IO as I

import System.FilePath
  (
    splitDirectories,
    joinPath,
    (</>)
  )
import Data.Time.Clock.POSIX
import Data.Time
import Files.Data
import System.Posix.Files
import System.Posix.Types
import System.Directory
import Control.Monad
import Data.Time.Format

stringOfTime :: POSIXTime -> String
stringOfTime time =
    formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime time)

makeTime :: EpochTime ->
            String
makeTime = show . posixSecondsToUTCTime . realToFrac

       -----------
       -- PATHS --
       -----------
curDirPath :: FilePath ->
              FilePath
curDirPath = last . splitDirectories

upDirPath :: FilePath ->
             FilePath
upDirPath = joinPath . init . splitDirectories