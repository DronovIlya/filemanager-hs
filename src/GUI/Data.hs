module GUI.Data where

import Control.Concurrent.STM (TVar)
import Graphics.UI.Gtk
import Files.Manager

data MyWindow = MyWindow {
  view :: TVar TreeView,
  path :: TVar FilePath,
  rawModel :: TVar (ListStore FileInfo)
}

data MyView = MyView {
  leftWindow :: MyWindow,
  rightWindow :: MyWindow
}

data MyGui = MyGui {
  rootWindow :: Window,
  scrollWindow1 :: ScrolledWindow,
  scrollWindow2 :: ScrolledWindow,

  -- Menu --
  actionMenu :: Menu,
  actionFileOpen :: ImageMenuItem,
  actionFileExecute :: ImageMenuItem,
  actionFileNew :: ImageMenuItem,
  actionFileCut :: ImageMenuItem,
  actionFileCopy :: ImageMenuItem,
  actionFileRename :: ImageMenuItem,
  actionFilePaste :: ImageMenuItem,
  actionFileDelete :: ImageMenuItem
}

