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
                      MyContainer -> 
                      IO ()
setEventsCallbacks gui container = do
  print "setup callbacks for container"

  leftView <- readTVarIO $ (view (left container))
  rightView <- readTVarIO $ (view (right container))
  _ <- leftView `on` rowActivated
    $ (\_ _ -> handleEvent gui (left container) (right container) open)

  _ <- rightView `on` rowActivated
    $ (\_ _ -> handleEvent gui (right container) (left container) open)

  handleGuiEvents gui (left container) (right container)
  handleGuiEvents gui (right container) (left container)
  return ()

handleGuiEvents :: MyGui -> 
                   MyView -> 
                   MyView -> 
                   IO ()
handleGuiEvents gui from to = do
  view <- readTVarIO $ (view from)
  -- handle mouse right-click
  _ <- view `on` buttonPressEvent $ do
    eb <- eventButton
    t <- eventTime
    case eb of
      RightButton -> liftIO $ menuPopup (actionMenu gui) $ Just (RightButton, t) 
      _ -> return ()
    return False

  _ <- actionFileOpen gui `on` menuItemActivated $
    liftIO $ handleEvent gui from to open
  _ <- actionFileCopy gui `on` menuItemActivated $
    liftIO $ handleEvent gui from to copy
  return ()
  

handleEvent :: MyGui -> 
               MyView -> 
               MyView -> 
               ([DataType] -> MyGui -> MyView -> MyView -> IO()) -> 
               IO ()
handleEvent gui from to func = do
  items <- getSelectedItems gui from
  func items gui from to

-- |Opens a file or directory
open :: [DataType] -> MyGui -> MyView -> MyView -> IO()
open [file] gui fromWindow toWindow = do
  --path <- readTVarIO $ path fromWindow
  --path' <- createPath path file
  --refreshWindow gui fromWindow path'
  print "open"
  return ()

-- |Copy files from one directory to another
-- TODO: check cases with incompatible files selection
copy :: [DataType] -> MyGui -> MyView -> MyView -> IO()
copy [file] gui fromWindow toWindow = do
  print "copy"
  return ()
--  fromPath <- readTVarIO $ path fromWindow
--  print fromPath
--  fromPath' <- createPath fromPath file
--  print fromPath'

--  toPath <- readTVarIO $ path toWindow
--  print toPath
--  toPath' <- createPath toPath file
--  print toPath'

--  copyFile fromPath' toPath'
--  refreshWindow gui toWindow toPath
--copy [] gui fromWindow toWindow = do
--  return ()

delete :: [DataType] -> MyGui -> MyView -> MyView -> IO()
delete [file] gui fromWindow toWindow = do
  print "delete"
  return ()

upDirectory :: MyGui -> MyView -> IO ()
upDirectory gui window = do
  print "upDirectory"
  return ()

renameFile :: [DataType] -> MyGui -> MyView -> IO ()
renameFile [file] gui window = do
  print "renameFile"
  return ()