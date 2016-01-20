module GUI.MyGui where

import Graphics.UI.Gtk
import GUI.Data

project_path = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/"

createGUI :: IO MyGui
createGUI = do
  builder <- builderNew
  builderAddFromFile builder (project_path ++ "res/layout.glade")
 
  rootWindow    <- builderGetObject builder castToWindow "rootWindow"
  scrollWindow1 <- builderGetObject builder castToScrolledWindow "scrollWindow1"
  --scrollWindow2 <- builderGetObject builder castToScrolledWindow "scrollWindow2"

  --return (MyGui rootWindow scrollWindow1 scrollWindow2)
  return (MyGui rootWindow scrollWindow1)
