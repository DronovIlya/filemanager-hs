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
  right <- readTVarIO $ (view (rightWindow myview))
  _ <- left `on` rowActivated
    $ (\_ _ -> handleEvent gui (leftWindow myview) (rightWindow myview) open)

  _ <- right `on` rowActivated
    $ (\_ _ -> handleEvent gui (rightWindow myview) (leftWindow myview) open)

  handleGuiEvents gui (leftWindow myview) (rightWindow myview)
  handleGuiEvents gui (rightWindow myview) (leftWindow myview)
  return ()

handleGuiEvents :: MyGui -> MyWindow -> MyWindow -> IO ()
handleGuiEvents gui fromWindow toWindow = do
  view <- readTVarIO $ (view fromWindow)
  -- handle mouse right-click
  _ <- view `on` buttonPressEvent $ do
    eb <- eventButton
    t <- eventTime
    case eb of
      RightButton -> liftIO $ menuPopup (actionMenu gui) $ Just (RightButton, t) 
      _ -> return ()
    return False

  _ <- actionFileOpen gui `on` menuItemActivated $
    liftIO $ handleEvent gui fromWindow toWindow open
  _ <- actionFileExecute gui `on` menuItemActivated $
    liftIO $ handleEvent gui fromWindow toWindow execute
  _ <- actionFileCopy gui `on` menuItemActivated $
    liftIO $ handleEvent gui fromWindow toWindow copy
  return ()
  

handleEvent :: MyGui -> MyWindow -> MyWindow -> ([FileInfo] -> MyGui -> MyWindow -> MyWindow -> IO()) -> IO()
handleEvent gui fromWindow toWindow io = do
  items <- getSelectedItems gui fromWindow
  io items gui fromWindow toWindow

-- |Opens a file or directory
open :: [FileInfo] -> MyGui -> MyWindow -> MyWindow -> IO ()
open [file] gui fromWindow toWindow = do
  path <- readTVarIO $ path fromWindow
  path' <- createPath path file
  refreshWindow gui fromWindow path'
  return ()

-- |Copy files from one directory to another
-- TODO: check cases with incompatible files selection
copy :: [FileInfo] -> MyGui -> MyWindow -> MyWindow -> IO ()
copy [file] gui fromWindow toWindow = do
  fromPath <- readTVarIO $ path fromWindow
  print fromPath
  fromPath' <- createPath fromPath file
  print fromPath'

  toPath <- readTVarIO $ path toWindow
  print toPath
  toPath' <- createPath toPath file
  print toPath'

  copyFile fromPath' toPath'
  refreshWindow gui toWindow toPath
copy [] gui fromWindow toWindow = do
  return ()

execute :: [FileInfo] -> MyGui -> MyWindow -> MyWindow -> IO ()
execute [file] gui fromWindow toWindow = do
  print "execute"
  return ()

delete :: [FileInfo] -> MyGui -> MyWindow -> MyWindow -> IO ()
delete [file] gui fromWindow toWindow = do
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