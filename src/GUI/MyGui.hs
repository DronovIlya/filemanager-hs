module GUI.MyGui where

import Graphics.UI.Gtk
import IO.Utils
import GUI.Data
import Graphics.UI.Gtk.Gdk.Pixbuf
import Paths_filemanager_hs
  (
    getDataFileName
  )

projectPath = "/Users/ilya.dronov/ifmo/haskell/course/filemanager-hs/"
imagesPath = projectPath ++ "res/images"

data Icon = TFolder | TFile | TUnkhown 

createIcon :: Icon ->
              IO Pixbuf
createIcon icon = pixbufNewFromFile =<< getDataFileName ("res/images/" ++ (iconPath icon))
  where
    iconPath TFolder     = "directory.png"
    iconPath TFile       = "file.png"

createGUI :: IO MyGui
createGUI = do
  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName ("res/layout.glade")
 
  folderIcon <- createIcon TFolder
  fileIcon <- createIcon TFile

  rootWindow    <- builderGetObject builder castToWindow "rootWindow"
  scrollWindow1 <- builderGetObject builder castToScrolledWindow "scrolledwindow1"
  scrollWindow2 <- builderGetObject builder castToScrolledWindow "scrolledwindow2"
  statusBar1     <- builderGetObject builder castToStatusbar "statusBar1"
  statusBar2     <- builderGetObject builder castToStatusbar "statusBar2"

  menuChangeHidden     <- builderGetObject builder castToImageMenuItem "menuChangeHidden"

  actionMenu <- builderGetObject builder castToMenu "actionMenu"
  actionFileOpen <- builderGetObject builder castToImageMenuItem "actionFileOpen"
  actionFileCopy <- builderGetObject builder castToImageMenuItem "actionFileCopy"
  actionFileDelete <- builderGetObject builder castToImageMenuItem "actionFileDelete"

  showHidden <- newVar False
  return (MyGui rootWindow scrollWindow1 scrollWindow2 statusBar1 statusBar2 menuChangeHidden actionMenu actionFileOpen 
    actionFileCopy actionFileDelete showHidden folderIcon fileIcon)

