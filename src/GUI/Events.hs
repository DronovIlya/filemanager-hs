module GUI.Events where

import Control.Applicative((<$>))
import Control.Monad
import Control.Concurrent.STM(readTVarIO)
import Data.Maybe(catMaybes, fromJust)
import Graphics.UI.Gtk
import GUI.Data
import GUI.MyView
import Files.Manager
import System.Directory

setEventsCallbacks :: MyGui -> MyView -> IO ()
setEventsCallbacks gui myview = do
  print "setup callbacks"
  view <- readTVarIO $ view myview
  _ <- view `on` rowActivated
    $ (\_ _ -> handleEvent gui myview open)
  return ()

handleEvent :: MyGui -> MyView -> ([FileInfo] -> MyGui -> MyView -> IO()) -> IO()
handleEvent gui view io = do
  items <- getSelectedItems gui view
  io items gui view

getSelectedItems :: MyGui -> MyView -> IO [FileInfo]
getSelectedItems gui view = do
  tps <- getSelectedTreePaths gui view
  getSelectedItems' gui view tps

getSelectedItems' :: MyGui -> MyView -> [TreePath] -> IO [FileInfo]
getSelectedItems' gui view tps = do
  rawModel' <- readTVarIO $ rawModel view
  iters <- catMaybes <$> mapM (treeModelGetIter rawModel') tps
  forM iters $ \iter -> do
    treeModelGetRow rawModel' iter

getSelectedTreePaths :: MyGui -> MyView -> IO [TreePath]
getSelectedTreePaths gui myview = do
  view' <- readTVarIO $ view myview
  tvs <- treeViewGetSelection view'
  treeSelectionGetSelectedRows tvs 


open :: [FileInfo] -> MyGui -> MyView -> IO ()
open [file] gui view = do
  dir <- getCurrentDirectory
  refreshView' gui view (dir ++ "/" ++ (name file))
  return ()