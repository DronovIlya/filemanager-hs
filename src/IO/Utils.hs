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
newVar var = newTVarIO var

readVar :: TVar a -> 
           IO a
readVar var = readTVarIO var

writeVar :: TVar a -> 
            a -> 
            IO ()
writeVar tvar val = atomically $ writeTVar tvar val  
