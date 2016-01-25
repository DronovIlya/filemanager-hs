module Files.Utils where

import qualified Control.Exception as E
import qualified System.IO as I

import System.FilePath ( splitDirectories, joinPath, (</>) )
import Data.Default

import Data.Time.Clock.POSIX
import Data.Time
import Files.Data
import System.Posix.Files
import System.Posix.Types
import System.Directory
import Control.Monad
import Data.Time.Format

import qualified System.Posix.Files as SPF

stringOfTime :: POSIXTime -> 
                String
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


-- |A `maybe` flavor using the `Default` class.
maybeD :: (Default b) => (a -> b) -> Maybe a -> b
maybeD = maybe def


-- |Gets the contente variable. Returns Nothing if the constructor is of `Unknown`.
getFreeVar :: File a -> 
              Maybe a
getFreeVar (Directory _  d)       = Just d
getFreeVar (RegularFile _  d)     = Just d
getFreeVar _                      = Nothing

-- |Apply a function on the content variable. If there is no content variable
-- for the given constructor the value from the `Default` class is used.
fromFreeVar :: (Default d) => (a -> d) -> File a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


-- |Pack the modification time into a string.
packModTime :: File FileInfo
            -> String
packModTime = fromFreeVar $ show . posixSecondsToUTCTime . realToFrac . time


-- |Pack the permissions into a string, similar to what "ls -l" does.
packPermissions :: File FileInfo
                -> String
packPermissions dt = fromFreeVar (pStr . mode) dt
  where
    pStr ffm = typeModeStr ++ ownerModeStr ++ groupModeStr ++ otherModeStr
      where
        typeModeStr = case dt of
          Directory {}       -> "d"
          RegularFile {}   -> "-"
        ownerModeStr =    hasFmStr SPF.ownerReadMode    "r"
                       ++ hasFmStr SPF.ownerWriteMode   "w"
                       ++ hasFmStr SPF.ownerExecuteMode "x"
        groupModeStr =    hasFmStr SPF.groupReadMode    "r"
                       ++ hasFmStr SPF.groupWriteMode   "w"
                       ++ hasFmStr SPF.groupExecuteMode "x"
        otherModeStr =    hasFmStr SPF.otherReadMode    "r"
                       ++ hasFmStr SPF.otherWriteMode   "w"
                       ++ hasFmStr SPF.otherExecuteMode "x"
        hasFmStr fm str
          | hasFM fm  = str
          | otherwise = "-"
        hasFM fm = ffm `SPF.intersectFileModes` fm == fm

