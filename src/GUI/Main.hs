module Main where

import Graphics.UI.Gtk
import GUI.MyGui (createGUI)
import GUI.MyView
import GUI.Data

main :: IO ()
main = do
  initGUI

  myGui <- createGUI
  myView <- createMyView myGui createTreeView  

  refreshView myGui myView

  widgetShowAll (rootWindow myGui)
  
  mainGUI
  return ()