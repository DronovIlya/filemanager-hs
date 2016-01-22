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
import System.Directory (setCurrentDirectory)

createMyView :: MyGui -> IO (TreeView) -> IO (MyView)
createMyView gui iotv = do
  treeView <- iotv

  list <- newTVarIO =<< listStoreNew []
  
  treeView' <- newTVarIO treeView
  let myView = MyView treeView' list

  setEventsCallbacks gui myView 
  containerAdd (scrollWindow1 gui) treeView

  return myView

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
  setCurrentDirectory fp
  files <- createFileInfo =<< obtainDirectory fp
  filesList <- listStoreNew files

  writeTVarIO (rawModel myview) filesList
  constructView mygui myview
  return ()

constructView :: MyGui -> MyView -> IO ()
constructView gui myview = do
  view' <- readTVarIO $ view myview
  rawModel' <- readTVarIO $ rawModel myview

  treeModelSetColumn rawModel' (makeColumnIdString 0) name 
  treeModelSetColumn rawModel' (makeColumnIdString 1) size 
  treeModelSetColumn rawModel' (makeColumnIdString 2) permissions
  
  treeViewSetModel view' rawModel'
  treeViewSetRubberBanding view' True
  return ()