module GUI.MyGui where

import Graphics.UI.Gtk
import GUI.Data

project_path = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/"

createGUI :: IO MyGui
createGUI = do
  builder <- builderNew
  builderAddFromFile builder (project_path ++ "res/layout.glade")
 
  rootWindow    <- builderGetObject builder castToWindow "rootWindow"
  scrollWindow1 <- builderGetObject builder castToScrolledWindow "scrolledwindow1"
  scrollWindow2 <- builderGetObject builder castToScrolledWindow "scrolledwindow2"

  actionMenu <- builderGetObject builder castToMenu "actionMenu"
  actionFileOpen <- builderGetObject builder castToImageMenuItem "actionFileOpen"
  actionFileExecute <- builderGetObject builder castToImageMenuItem "actionFileExecute"
  actionFileNew <- builderGetObject builder castToImageMenuItem "actionFileNew"
  actionFileCut <- builderGetObject builder castToImageMenuItem "actionFileCut"
  actionFileCopy <- builderGetObject builder castToImageMenuItem "actionFileCopy"
  actionFileRename <- builderGetObject builder castToImageMenuItem "actionFileRename"
  actionFilePaste <- builderGetObject builder castToImageMenuItem "actionFilePaste"
  actionFileDelete <- builderGetObject builder castToImageMenuItem "actionFileDelete"

  return (MyGui rootWindow scrollWindow1 scrollWindow2 actionMenu actionFileOpen actionFileExecute
  	actionFileNew actionFileCut actionFileCopy actionFileRename actionFilePaste actionFileDelete)

