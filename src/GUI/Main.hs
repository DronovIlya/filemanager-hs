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

  constructView myGui myView

  set (rootWindow myGui) [ windowDefaultWidth := 500, windowDefaultHeight := 500 ]
  widgetShowAll (rootWindow myGui)
  
  mainGUI
  return ()