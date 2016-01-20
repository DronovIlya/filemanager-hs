module GUI.Data where

import Control.Concurrent.STM (TVar)
import Graphics.UI.Gtk
import Files.Manager

data MyView = MyView {
  view   :: TVar TreeView,
  rawModel :: TVar (ListStore FileInfo)
}

data MyGui = MyGui {
  rootWindow :: Window,
  scrollWindow1 :: ScrolledWindow
  --scrollWindow2 :: ScrolledWindow
}

