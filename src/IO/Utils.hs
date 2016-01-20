module IO.Utils where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically)

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO tvar val = atomically $ writeTVar tvar val  
