
-- |This module provides basis types on working with GUI

module GUI.Data where

import Control.Concurrent.STM (TVar)
import Graphics.UI.Gtk
import Files.Data

-- |Data type that stores in views
type DataType = FileEntry FileInfo

-- |Container that holds current view's state
data MyContainer = MyContainer {
  left  :: MyView,                           -- ^ left view
  right :: MyView                            -- ^ right view
}

-- |Model that contains neccesary data
data MyView = MyView {
  view     :: TVar TreeView,                 -- ^ main view container
  dir      :: TVar DataType,                 -- ^ current directory 
  rawModel :: TVar (ListStore DataType)      -- ^ corresponding data
}

-- |Base GUI data
data MyGui = MyGui {
  rootWindow        :: Window,               -- ^ root window - GtkWindow
  scrollWindow1     :: ScrolledWindow,       -- ^ left scrolled window
  scrollWindow2     :: ScrolledWindow,       -- ^ right scrolled window
  statusBar1        :: Statusbar,
  statusBar2        :: Statusbar,

  menuChangeHidden  :: ImageMenuItem,

  actionMenu        :: Menu,                 -- ^ action menu appears on right click
  actionFileOpen    :: ImageMenuItem,        -- ^ open file action
  actionFileCopy    :: ImageMenuItem,        -- ^ copy file action
  actionFileDelete  :: ImageMenuItem,        -- ^ delete file action
  actionFileNew     :: ImageMenuItem,        -- ^ new file action action

  -- Support information
  showHidden :: TVar Bool,                   -- ^ hiddent state of all files
  
  folderIcon :: Pixbuf,                      -- ^ icon for directory
  fileIcon   :: Pixbuf                       -- ^ icon for file
}

