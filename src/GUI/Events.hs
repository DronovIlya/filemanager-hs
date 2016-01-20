module GUI.Events where

import Control.Applicative((<$>))
import Control.Monad
import Control.Concurrent.STM(readTVarIO)
import Data.Maybe(catMaybes, fromJust)
import Graphics.UI.Gtk
import GUI.Data
import GUI.MyView
import Files.Manager

setEventsCallbacks :: MyGui -> MyView -> IO ()
setEventsCallbacks gui myview = do
  print "callbacks"
  view <- readTVarIO $ view myview
  _ <- view `on` rowActivated
    $ (\_ _ -> handleEvent gui myview open)
  return ()

getSelectedTreePaths :: MyGui -> MyView -> IO [TreePath]
getSelectedTreePaths gui myview = do
  view' <- readTVarIO $ view myview
  tvs <- treeViewGetSelection view'
  treeSelectionGetSelectedRows tvs 


getSelectedItems' :: MyGui -> MyView -> [TreePath] -> IO [FileInfo]
getSelectedItems' gui view tps = do
  rawModel' <- readTVarIO $ rawModel view
  iters <- catMaybes <$> mapM (treeModelGetIter rawModel') tps
  forM iters $ \iter -> do
    treeModelGetRow rawModel' iter

getSelectedItems :: MyGui -> MyView -> IO [FileInfo]
getSelectedItems gui view = do
  tps <- getSelectedTreePaths gui view
  getSelectedItems' gui view tps

handleEvent :: MyGui -> MyView -> ([FileInfo] -> MyGui -> MyView -> IO()) -> IO()
handleEvent gui view io = do
  items <- getSelectedItems gui view
  io items gui view

open :: [FileInfo] -> MyGui -> MyView -> IO ()
open files gui view = do
  print "success"
  return ()