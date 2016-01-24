module GUI.MyGui where

import Graphics.UI.Gtk
import IO.Utils
import GUI.Data

projectPath = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/"

createGUI :: IO MyGui
createGUI = do
  builder <- builderNew
  builderAddFromFile builder (projectPath ++ "res/layout.glade")
 
  rootWindow    <- builderGetObject builder castToWindow "rootWindow"
  scrollWindow1 <- builderGetObject builder castToScrolledWindow "scrolledwindow1"
  scrollWindow2 <- builderGetObject builder castToScrolledWindow "scrolledwindow2"

  actionMenu <- builderGetObject builder castToMenu "actionMenu"
  actionFileOpen <- builderGetObject builder castToImageMenuItem "actionFileOpen"
  actionFileCopy <- builderGetObject builder castToImageMenuItem "actionFileCopy"
  actionFileDelete <- builderGetObject builder castToImageMenuItem "actionFileDelete"

  showHidden <- newVar False
  return (MyGui rootWindow scrollWindow1 scrollWindow2 actionMenu actionFileOpen 
    actionFileCopy actionFileDelete showHidden)

