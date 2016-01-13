module GUI.MyView where

import Control.Applicative
  (
    (<$>)
  )
import Graphics.UI.Gtk
import GUI.Data

names :: IO [(String, String)]
names = return [("123", "123"), ("234", "456")]

createMyView :: MyGui -> IO (TreeView) -> IO (MyView)
createMyView gui iotv = do
  treeView <- iotv

  list <- listStoreNew =<< names

  containerAdd (scrollWindow1 gui) treeView
  
  let myView = MyView treeView list
  return (MyView treeView list)

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

getRawModel :: MyView -> IO (ListStore (String, String))
getRawModel myview = return (models myview)

constructView :: MyGui -> MyView -> IO ()
constructView gui myview = do
  rawModel <- getRawModel myview
  --sortedModel <- treeModelSortNewWithModel rawModel []

  treeModelSetColumn rawModel (makeColumnIdString 0) fst 
  treeModelSetColumn rawModel (makeColumnIdString 1) fst 
  treeModelSetColumn rawModel (makeColumnIdString 2) fst 
  
  treeViewSetModel (view myview) rawModel
  treeViewSetRubberBanding (view myview) True
  return ()