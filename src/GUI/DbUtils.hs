{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies       #-}

module GUI.DbUtils where

import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Data.Acid.Remote
import Control.Monad
import Control.Monad.Reader (ask)

import qualified Data.Map as Map
import qualified Control.Monad.State as S

type Key = String
type Value = String

data Database = Database !(Map.Map Key Value)
    deriving (Show, Ord, Eq, Typeable)

projectPath = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/db/"

$(deriveSafeCopy 0 'base ''Database)

insertKey :: Key -> Value -> Update Database ()
insertKey key value
    = do Database m <- S.get
         S.put (Database (Map.insert key value m))

lookupKey :: Key -> Query Database (Maybe Value)
lookupKey key
    = do Database m <- ask
         return (Map.lookup key m)

$(makeAcidic ''Database ['insertKey, 'lookupKey])

fixtures :: Map.Map String String
fixtures = Map.empty

insert :: Key -> Value -> IO ()
insert key val = do
  database <- openLocalStateFrom projectPath (Database fixtures)
  _ <- update database (InsertKey key val)
  closeAcidState database

get :: Key -> IO (Maybe Value)
get key = do
  database <- openLocalStateFrom projectPath (Database fixtures)
  result <- query database (LookupKey key)
  closeAcidState database
  return result
 