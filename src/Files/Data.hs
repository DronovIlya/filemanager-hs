module Files.Data where


import System.Posix.Types 
  (
	  EpochTime
  )

import Control.Exception.Base
  (
    IOException
  )
  
type FileName = String

data FileEntry a = FileEntry {
    path :: FilePath,        -- ^ parent directory path
    file :: File a           -- ^ file content
  }

data File a =
    Directory {
    name :: FileName,        -- ^ directory name
    content :: a                -- ^ file content
  }
  | RegularFile {
    name :: FileName,        -- ^ regular file name
    content :: a                -- ^ file content
  }
  | UnkhownFile {
    name :: FileName,        -- ^ unkhown file name
    exception :: IOException -- ^ exception while parsing file
  }

data FileInfo = FileInfo {
  modificationTime :: EpochTime -- ^ file modification time
}
