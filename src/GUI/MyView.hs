module GUI.MyView where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, atomically)
import Control.Monad
import Graphics.UI.Gtk
import GUI.Data
import GUI.Utils
import IO.Utils
-- fix "cycle" probrem at compile time
import {-# SOURCE #-} GUI.Events (setEventsCallbacks)
import Files.Manager
import System.Directory

initDirectory :: IO ()
initDirectory = do
  home <- getHomeFolder
  setCurrentDirectory home
  return ()

createMyView :: MyGui -> IO (MyView)
createMyView gui = do
  leftTreeView <- createTreeView
  rightTreeView <- createTreeView

  window1 <- initTreeView (scrollWindow1 gui) leftTreeView
  window2 <- initTreeView (scrollWindow2 gui) rightTreeView

  let myview = MyView window1 window2
  setEventsCallbacks gui myview
  return myview

initTreeView :: ScrolledWindow -> TreeView -> IO (MyWindow)
initTreeView container tv = do
  rawModel <- newTVarIO =<< listStoreNew []
  path <- newTVarIO =<< getCurrentDirectory
  treeView <- newTVarIO tv
  containerAdd container tv
  return (MyWindow treeView path rawModel)
  
createTreeView :: IO (TreeView)
createTreeView = do
  treeView <- treeViewNew
    
  selection <- treeViewGetSelection treeView
  treeSelectionSetMode selection SelectionMultiple

  renderTxt <- cellRendererTextNew
  let ct = cellText   :: (CellRendererTextClass cr) => Attr cr String

  -- filename column
  cF <- treeViewColumnNew
  treeViewColumnSetTitle        cF "Filename"
  treeViewColumnSetResizable    cF True
  treeViewColumnSetClickable    cF True
  treeViewColumnSetSortColumnId cF 0
  cellLayoutPackStart cF renderTxt True
  _ <- treeViewAppendColumn treeView cF
  cellLayoutAddColumnAttribute cF renderTxt ct $ makeColumnIdString 0

  -- date column
  cMD <- treeViewColumnNew
  treeViewColumnSetTitle        cMD "Date"
  treeViewColumnSetResizable    cMD True
  treeViewColumnSetClickable    cMD True
  treeViewColumnSetSortColumnId cMD 1
  cellLayoutPackStart cMD renderTxt True
  _ <- treeViewAppendColumn treeView cMD
  cellLayoutAddColumnAttribute cMD renderTxt ct $ makeColumnIdString 1

  -- permissions column
  cP <- treeViewColumnNew
  treeViewColumnSetTitle        cP "Permission"
  treeViewColumnSetResizable    cP True
  treeViewColumnSetClickable    cP True
  treeViewColumnSetSortColumnId cP 2
  cellLayoutPackStart cP renderTxt True
  _ <- treeViewAppendColumn treeView cP
  cellLayoutAddColumnAttribute cP renderTxt ct $ makeColumnIdString 2

  return treeView

refreshView :: MyGui -> MyView -> Maybe FilePath -> IO ()
refreshView mygui myview mfp = do
  case mfp of
    Just fp -> refreshView' mygui myview fp
    Nothing -> refreshView' mygui myview =<< getHomeFolder

refreshView' :: MyGui -> MyView -> FilePath -> IO ()
refreshView' mygui myview fp = do
  refreshWindow mygui (leftWindow myview) fp
  refreshWindow mygui (rightWindow myview) fp
  return ()

refreshWindow :: MyGui -> MyWindow -> FilePath -> IO ()
refreshWindow gui window fp = do
  print "start refresh window"
  setCurrentDirectory fp
  print "setdirectroy"
  files <- createFileInfo =<< obtainDirectory fp
  filesList <- listStoreNew files
  
  writeTVarIO (rawModel window) filesList
  writeTVarIO (path window) fp
  print "start constructView"
  constructView gui window

constructView :: MyGui -> MyWindow -> IO ()
constructView gui myview = do
  view' <- readTVarIO $ view myview
  rawModel' <- readTVarIO $ rawModel myview

  treeModelSetColumn rawModel' (makeColumnIdString 0) name 
  treeModelSetColumn rawModel' (makeColumnIdString 1) size 
  treeModelSetColumn rawModel' (makeColumnIdString 2) permissions
  
  treeViewSetModel view' rawModel'
  treeViewSetRubberBanding view' True
  return ()