module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import GUI.MyGui (createGUI)
import GUI.MyView
import Control.Monad
import IO.Utils
import GUI.Data
import Files.Manager
import GUI.DbUtils 
  (
  	insert,
  	get
  )

main :: IO ()
main = do
  initGUI

  initDirectory

  gui <- createGUI
  container <- createBaseContainer gui 

  (leftPath, rightPath) <- restoreState
  refreshContainer gui container leftPath rightPath

  widgetShowAll (rootWindow gui)
  
  _ <- (rootWindow gui) `on` deleteEvent $
    liftIO $ saveState container >> return False

  mainGUI
  return ()

saveState :: MyContainer ->
             IO ()
saveState container = do
  leftDir  <- readVar $ dir (left container)
  rightDir <- readVar $ dir (right container)

  GUI.DbUtils.insert "leftDir"  $ Files.Manager.getFullPath leftDir
  GUI.DbUtils.insert "rightDir" $ Files.Manager.getFullPath rightDir

restoreState :: IO ((Maybe String, Maybe String))
restoreState = do
  leftPath  <- GUI.DbUtils.get("leftDir")
  rightPath <- GUI.DbUtils.get("rightDir")
  return (leftPath, rightPath) 