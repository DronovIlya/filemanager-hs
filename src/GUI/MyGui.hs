module GUI.MyGui where

import Graphics.UI.Gtk
import GUI.Data

project_path = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/"

createGUI :: IO MyGui
createGUI = do
  builder <- builderNew
  builderAddFromFile builder (project_path ++ "res/layout.xml")
 
  rootWindow    <- builderGetObject builder castToWindow "rootWin"
  scrollWindow1 <- builderGetObject builder castToScrolledWindow "mainScroll"
  scrollWindow2 <- builderGetObject builder castToScrolledWindow "mainScroll"

  return (MyGui rootWindow scrollWindow1 scrollWindow2)
