module Main where

import Graphics.UI.Gtk
import GUI.MyGui (createGUI)
import GUI.MyView
import GUI.Data

main :: IO ()
main = do
  initGUI

  initDirectory

  gui <- createGUI
  container <- createBaseContainer gui 

  refreshContainer gui container Nothing

  widgetShowAll (rootWindow gui)
  
  mainGUI
  return ()