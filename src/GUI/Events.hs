module GUI.Events where

import Control.Applicative
  (
    (<$>)
  )

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
  (
    catMaybes, 
    fromJust
  )
import Graphics.UI.Gtk
import GUI.Data
import GUI.MyView
import GUI.Utils
import GUI.Popup
import IO.Utils
import Files.Manager
  (
    readFile,
    getFullPath
  )
import Files.Operations
  (
    copy
  )

import Control.Concurrent
import Files.Data
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
  leftView <- readVar $ (view (left container))
  rightView <- readVar $ (view (right container))

  _ <- leftView `on` rowActivated
    $ (\_ _ -> handleEvent gui (left container) (right container) openEvent)

  _ <- rightView `on` rowActivated
    $ (\_ _ -> handleEvent gui (right container) (left container) openEvent)

  handleGuiEvents gui (left container) (right container)
  handleGuiEvents gui (right container) (left container)
  return ()

handleGuiEvents :: MyGui -> 
                   MyView -> 
                   MyView -> 
                   IO ()
handleGuiEvents gui from to = do
  view <- readVar $ (view from)
  -- handle mouse right-click
  _ <- view `on` buttonPressEvent $ do
    eb <- eventButton
    t <- eventTime
    case eb of
      RightButton -> liftIO $ menuPopup (actionMenu gui) $ Just (RightButton, t) 
      _ -> return ()
    return False

  _ <- actionFileOpen gui `on` menuItemActivated $
    liftIO $ handleEvent gui from to openEvent
  _ <- actionFileCopy gui `on` menuItemActivated $
    liftIO $ handleEvent gui from to copyEvent
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
openEvent :: [DataType] -> 
             MyGui -> 
             MyView -> 
             MyView -> 
             IO ()
openEvent [file] gui from to = do
  case file of
    IsDir f -> do
      ff <- Files.Manager.readFile $ getFullPath file
      refreshView gui from ff
    f -> do
      return ()
openEvent _ _ _ _ = return ()

-- |Copy files from one directory to another
-- TODO: check cases with incompatible files selection
copyEvent :: [DataType] -> 
             MyGui -> 
             MyView -> 
             MyView -> 
             IO()
copyEvent [file] gui from to = catchError $ do
  toDir <- readVar $ dir to
  forkIO $ do
    Files.Operations.copy file toDir
    postGUIAsync $ refreshView gui to toDir
  return ()
copyEvent _ _ _ _ = return ()

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