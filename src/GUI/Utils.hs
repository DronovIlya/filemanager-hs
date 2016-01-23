module GUI.Utils where

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM(readTVarIO)
import Data.Maybe(catMaybes, fromJust)
import Graphics.UI.Gtk
import GUI.Data
import Files.Manager
import System.Directory

getSelectedItems :: MyGui -> MyWindow -> IO [FileInfo]
getSelectedItems gui window = do
  tps <- getSelectedTreePaths gui window
  getSelectedItems' gui window tps

getSelectedItems' :: MyGui -> MyWindow -> [TreePath] -> IO [FileInfo]
getSelectedItems' gui window tps = do
  rawModel' <- readTVarIO $ rawModel window
  iters <- catMaybes <$> mapM (treeModelGetIter rawModel') tps
  forM iters $ \iter -> do
    treeModelGetRow rawModel' iter

getSelectedTreePaths :: MyGui -> MyWindow -> IO [TreePath]
getSelectedTreePaths gui window = do
  view' <- readTVarIO $ view window
  tvs <- treeViewGetSelection view'
  treeSelectionGetSelectedRows tvs 