module GUI.MyView where

import Control.Monad
import Graphics.UI.Gtk
import GUI.Data
import GUI.Utils
import GUI.MyGui
import IO.Utils ( newVar, readVar, writeVar )
-- fix "cycle" probrem at compile time
import {-# SOURCE #-} GUI.Events ( setEventsCallbacks, refreshStatusBar )
import Files.Manager ( readFile, getHomeFolder, isHidden )
import Files.Utils
import Files.Data
import System.Directory

-- |Initialize base directory in case of unkhown previous state. Actual on first run
initDirectory :: IO ()
initDirectory = do
  home <- Files.Manager.getHomeFolder
  setCurrentDirectory home
  return ()

-- | Create base container of 2 windows. Create it and set events callbacks
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

-- | Create view and add it to UI hierarchy. At first time, it fills the empty data
createMyView :: ScrolledWindow -> 
                TreeView -> 
                IO MyView
createMyView container tv = do
  rawModel <- newVar =<< listStoreNew []
  dir <- newVar =<< Files.Manager.readFile =<< getCurrentDirectory
  treeView <- newVar tv
  containerAdd container tv
  return (MyView treeView dir rawModel)
  
-- | Create TreeView holder for store out data. 
-- Set multiple selection and create 3 columns : FileName, Date and Permission
createTreeView :: IO TreeView
createTreeView = do
  treeView <- treeViewNew
    
  selection <- treeViewGetSelection treeView
  treeSelectionSetMode selection SelectionMultiple

  createTreeViewIconColumn treeView "Filename" 0 1
  createTreeViewColumn treeView "Date" 2
  createTreeViewColumn treeView "Permission" 3
  return treeView

-- | Create tree view column with icon
createTreeViewIconColumn :: TreeView ->
                            String ->
                            Int ->
                            Int ->
                            IO ()
createTreeViewIconColumn tv title i1 i2 = do
  renderT <- cellRendererTextNew
  renderP <- cellRendererPixbufNew
  let cellT = cellText   :: (CellRendererTextClass cr) => Attr cr String
      cellBuf = cellPixbuf :: (CellRendererPixbufClass self) => Attr self Pixbuf
  
  column <- createColumn title i2
  cellLayoutPackStart column renderP False
  cellLayoutPackStart column renderT True
  _ <- treeViewAppendColumn tv column
  cellLayoutAddColumnAttribute column renderP cellBuf $ makeColumnIdPixbuf i1
  cellLayoutAddColumnAttribute column renderT cellT   $ makeColumnIdString i2


-- | Create tree view column without icon
createTreeViewColumn :: TreeView ->
                        String ->
                        Int ->
                        IO ()
createTreeViewColumn view title index = do
  renderTxt <- cellRendererTextNew
  let cell = cellText :: (CellRendererTextClass cr) => Attr cr String

  column <- createColumn title index
  cellLayoutPackStart column renderTxt True
  _ <- treeViewAppendColumn view column
  cellLayoutAddColumnAttribute column renderTxt cell $ makeColumnIdString index

  return ()

-- | Create a simple TreeView column
createColumn :: String ->
                Int ->
                IO TreeViewColumn
createColumn title i = do
  column <- treeViewColumnNew
  treeViewColumnSetTitle        column title
  treeViewColumnSetResizable    column True
  treeViewColumnSetClickable    column False
  treeViewColumnSetSortColumnId column i
  return column
-- | Refresh whole container : refresh left and right views and updates each status bar
refreshContainer :: MyGui -> 
                    MyContainer -> 
                    Maybe FilePath -> 
                    Maybe FilePath ->
                    IO ()
refreshContainer mygui container mfp1 mfp2 = do
  refreshContainer' mygui (left container) mfp1
  refreshContainer' mygui (right container) mfp2
  refreshStatusBar mygui container

-- | Refresh views on givent filepath. In case of unkhown filepath, 
-- uses home directory path
refreshContainer' :: MyGui -> 
                     MyView -> 
                     Maybe FilePath -> 
                     IO ()
refreshContainer' mygui view mfp = do
  case mfp of
    Just fp -> refreshView mygui view =<< Files.Manager.readFile fp
    Nothing -> refreshView mygui view =<< Files.Manager.readFile =<< getHomeFolder
  return ()

-- | Refresh view on current data
refreshViewState :: MyGui ->
                    MyView ->
                    IO ()
refreshViewState gui view = do
  dir <- readVar $ dir view
  refreshView gui view dir

-- | Refresh view on new data. Save new file state into the view, updates
-- file contents and GUI
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

-- | Setting model to the TreeView 
constructView :: MyGui -> 
                 MyView -> 
                 IO ()
constructView gui myview = do
  view' <- readVar $ view myview
  rawModel' <- readVar $ rawModel myview

  treeModelSetColumn rawModel' (makeColumnIdPixbuf 0) (getIcon . file)
  treeModelSetColumn rawModel' (makeColumnIdString 1) (name . file) 
  treeModelSetColumn rawModel' (makeColumnIdString 2) (packModTime . file)
  treeModelSetColumn rawModel' (makeColumnIdString 3) (packPermissions . file)
  
  treeViewSetModel view' rawModel'
  treeViewSetRubberBanding view' True
  return ()
  where
    getIcon Directory {}   = folderIcon gui
    getIcon RegularFile {} = fileIcon gui