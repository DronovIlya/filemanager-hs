-- |Main GUI module
module Main where

import Graphics.UI.Gtk
import Control.Monad.IO.Class
import GUI.MyGui ( createGUI )
import GUI.MyView
import Control.Monad
import IO.Utils
import GUI.Data
import Files.Manager
import GUI.DbUtils ( insert, get )

main :: IO ()
main = do
  initGUI

  initDirectory

  gui <- createGUI
  container <- createBaseContainer gui 

  -- restore saved state and fill views
  (leftPath, rightPath) <- restoreState
  refreshContainer gui container leftPath rightPath

  widgetShowAll (rootWindow gui)
  
  set (rootWindow gui) [windowTitle := "FileManager"]

  _ <- rootWindow gui `on` deleteEvent $
    liftIO $ saveState container >> return False

  mainGUI
  return ()

-- |Save container state into DB. Save only 2 paths that indicated left and right window
-- Saving state only on successful closure
saveState :: MyContainer ->
             IO ()
saveState container = do
  leftDir  <- readVar $ dir (left container)
  rightDir <- readVar $ dir (right container)
  print "saveState"
  print $ Files.Manager.getFullPath leftDir
  print $ Files.Manager.getFullPath rightDir
  GUI.DbUtils.insert "leftDir"  $ Files.Manager.getFullPath leftDir
  GUI.DbUtils.insert "rightDir" $ Files.Manager.getFullPath rightDir

  mainQuit

-- |Restore saved state from DB.
restoreState :: IO (Maybe String, Maybe String)
restoreState = do
  leftPath  <- GUI.DbUtils.get "leftDir"
  rightPath <- GUI.DbUtils.get "rightDir"
  return (leftPath, rightPath) 