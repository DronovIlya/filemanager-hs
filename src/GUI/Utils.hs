module GUI.Utils where

import Control.Applicative
  (
    (<$>)
  )
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
  (
    catMaybes, 
    fromJust
  )

import Graphics.UI.Gtk
import GUI.Data
import IO.Utils
import Files.Manager
  (
    obtainContents
  )

import Files.Data
import System.Directory

getSelectedItems :: MyGui -> 
                    MyView -> 
                    IO [DataType]
getSelectedItems gui view = do
  tps <- getSelectedTreePaths gui view
  getSelectedItems' gui view tps

getSelectedItems' :: MyGui -> 
                     MyView -> 
                     [TreePath] -> 
                     IO [DataType]
getSelectedItems' gui myview tps = do
  rawModel' <- readVar $ rawModel myview
  iters <- catMaybes <$> mapM (treeModelGetIter rawModel') tps
  forM iters $ \iter -> do
    treeModelGetRow rawModel' iter

getSelectedTreePaths :: MyGui -> 
                        MyView -> 
                        IO [TreePath]
getSelectedTreePaths gui myview = do
  view' <- readVar $ view myview
  tvs <- treeViewGetSelection view'
  treeSelectionGetSelectedRows tvs 


obtainListStore :: FileEntry FileInfo ->
                   IO (ListStore DataType)
obtainListStore ff = do
  content <- Files.Manager.obtainContents ff
  listStoreNew content