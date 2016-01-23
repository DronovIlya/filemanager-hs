module Main where

import Graphics.UI.Gtk
import GUI.MyGui (createGUI)
import GUI.MyView
import GUI.Data

main :: IO ()
main = do
  initGUI

  initDirectory

  myGui <- createGUI
  myView <- createMyView myGui 

  refreshView myGui myView Nothing

  widgetShowAll (rootWindow myGui)
  
  mainGUI
  return ()