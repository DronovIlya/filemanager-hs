module GUI.Data where

import Graphics.UI.Gtk

data FileInfo = FileInfo {
  name :: String
}

data MyView = MyView {
  view   :: TreeView,
  models :: ListStore (String, String) 
}

data MyGui = MyGui {
  rootWindow :: Window,
  scrollWindow1 :: ScrolledWindow,
  scrollWindow2 :: ScrolledWindow
}

