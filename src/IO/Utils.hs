module IO.Utils where

import Control.Concurrent.STM 
  (
	TVar, 
	newTVarIO,
	readTVarIO, 
	writeTVar, 
	atomically
  )

newVar :: a -> 
          IO (TVar a)
newVar = newTVarIO

readVar :: TVar a -> 
           IO a
readVar = readTVarIO

writeVar :: TVar a -> 
            a -> 
            IO ()
writeVar tvar val = atomically $ writeTVar tvar val  
