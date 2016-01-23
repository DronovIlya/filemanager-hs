module GUI.Events where

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM(readTVarIO)
import Data.Maybe(catMaybes, fromJust)
import Graphics.UI.Gtk
import GUI.Data
import GUI.MyView
import GUI.Utils
import Files.Manager
import System.Directory
import System.Glib.UTFString
  (
    glibToString
  )

-- | Set GUI events callback : clicks, actions
setEventsCallbacks :: MyGui -> 
                      MyView -> 
                      IO ()
setEventsCallbacks gui myview = do
  print "setup callbacks"
  left <- readTVarIO $ (view (leftWindow myview))
  _ <- left `on` rowActivated
    $ (\_ _ -> handleEvent gui (leftWindow myview) open)

  handleGuiEvents gui (leftWindow myview)
  return ()

handleGuiEvents :: MyGui -> MyWindow -> IO ()
handleGuiEvents gui window = do
  view <- readTVarIO $ (view window)
  -- handle mouse right-click
  _ <- view `on` buttonPressEvent $ do
    eb <- eventButton
    t <- eventTime
    case eb of
      RightButton -> liftIO $ menuPopup (actionMenu gui) $ Just (RightButton, t) 
      _ -> return ()
    return False

  _ <- actionFileOpen gui `on` menuItemActivated $
    liftIO $ handleEvent gui window open
  _ <- actionFileExecute gui `on` menuItemActivated $
    liftIO $ handleEvent gui window execute
  return ()
  

handleEvent :: MyGui -> MyWindow -> ([FileInfo] -> MyGui -> MyWindow -> IO()) -> IO()
handleEvent gui window io = do
  items <- getSelectedItems gui window
  io items gui window


open :: [FileInfo] -> MyGui -> MyWindow -> IO ()
open [file] gui window = do
  print "open"
  return ()

execute :: [FileInfo] -> MyGui -> MyWindow -> IO ()
execute [file] gui window = do
  print "execute"
  return ()

delete :: [FileInfo] -> MyGui -> MyWindow -> IO ()
delete [file] gui window = do
  print "delete"
  return ()

upDirectory :: MyGui -> MyWindow -> IO ()
upDirectory gui window = do
  print "upDirectory"
  return ()

renameFile :: [FileInfo] -> MyGui -> MyWindow -> IO ()
renameFile [file] gui window = do
  print "renameFile"
  return ()