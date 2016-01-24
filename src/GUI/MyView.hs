module GUI.MyView where

import Control.Monad
import Graphics.UI.Gtk
import GUI.Data
import GUI.Utils
import IO.Utils
  (
    newVar,
    readVar,
    writeVar
  )
-- fix "cycle" probrem at compile time
import {-# SOURCE #-} GUI.Events 
  (
    setEventsCallbacks
  )

import Files.Manager
  (
    readFile,
    getHomeFolder,
    isHidden
  )

import Files.Data
import System.Directory

initDirectory :: IO ()
initDirectory = do
  home <- Files.Manager.getHomeFolder
  setCurrentDirectory home
  return ()

createBaseContainer :: MyGui -> 
                       IO MyContainer
createBaseContainer gui = do
  leftTree <- createTreeView
  rightTree <- createTreeView

  left <- createMyView (scrollWindow1 gui) leftTree
  right <- createMyView (scrollWindow2 gui) rightTree

  let container = MyContainer left right

  setEventsCallbacks gui container
  return container

createMyView :: ScrolledWindow -> 
                TreeView -> 
                IO MyView
createMyView container tv = do
  rawModel <- newVar =<< listStoreNew []
  dir <- newVar =<< Files.Manager.readFile =<< getCurrentDirectory
  treeView <- newVar tv
  containerAdd container tv
  return (MyView treeView dir rawModel)
  
createTreeView :: IO TreeView
createTreeView = do
  treeView <- treeViewNew
    
  selection <- treeViewGetSelection treeView
  treeSelectionSetMode selection SelectionMultiple

  createTreeViewColumn treeView "Filename" 0
  createTreeViewColumn treeView "Date" 1
  createTreeViewColumn treeView "Permission" 2
  return treeView

createTreeViewColumn :: TreeView ->
                        String ->
                        Int ->
                        IO ()
createTreeViewColumn view title index = do
  renderTxt <- cellRendererTextNew
  let cell = cellText :: (CellRendererTextClass cr) => Attr cr String

  column <- treeViewColumnNew
  treeViewColumnSetTitle        column title
  treeViewColumnSetResizable    column True
  treeViewColumnSetClickable    column False
  treeViewColumnSetSortColumnId column index
  cellLayoutPackStart column renderTxt True
  _ <- treeViewAppendColumn view column
  cellLayoutAddColumnAttribute column renderTxt cell $ makeColumnIdString index

  return ()

refreshContainer :: MyGui -> 
                    MyContainer -> 
                    Maybe FilePath -> 
                    Maybe FilePath ->
                    IO ()
refreshContainer mygui container mfp1 mfp2 = do
  refreshContainer' mygui (left container) mfp1
  refreshContainer' mygui (right container) mfp2

refreshContainer' :: MyGui -> 
                     MyView -> 
                     Maybe FilePath -> 
                     IO ()
refreshContainer' mygui view mfp = do
  case mfp of
    Just fp -> refreshView mygui view =<< Files.Manager.readFile fp
    Nothing -> refreshView mygui view =<< Files.Manager.readFile =<< getHomeFolder
  return ()

refreshViewState :: MyGui ->
                    MyView ->
                    IO ()
refreshViewState gui view = do
  dir <- readVar $ dir view
  refreshView gui view dir

refreshView :: MyGui -> 
               MyView -> 
               FileEntry FileInfo -> 
               IO ()
refreshView gui myview ff = do
  setCurrentDirectory $ path ff
  showHidden <- readVar $ showHidden gui
  newRawModel <- obtainListStore ff showHidden
  
  writeVar (rawModel myview) newRawModel
  writeVar (dir myview) ff
  constructView gui myview

constructView :: MyGui -> 
                 MyView -> 
                 IO ()
constructView gui myview = do
  view' <- readVar $ view myview
  rawModel' <- readVar $ rawModel myview

  treeModelSetColumn rawModel' (makeColumnIdString 0) (name . file) 
  --treeModelSetColumn rawModel' (makeColumnIdString 1) size 
  --treeModelSetColumn rawModel' (makeColumnIdString 2) permissions
  
  treeViewSetModel view' rawModel'
  treeViewSetRubberBanding view' True
  return ()