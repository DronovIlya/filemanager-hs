module GUI.Popup where

import Control.Exception
  (
    catch,
    throw,
    try, 
    SomeException
   )
import Graphics.UI.Gtk

showErrorDialog :: String ->
                   IO ()
showErrorDialog s = do
  dialog <- messageDialogNew Nothing
                             [DialogDestroyWithParent]
                             MessageError
                             ButtonsClose
                             s
  _ <- dialogRun dialog
  widgetDestroy dialog

showProgressDialog :: String ->
                      IO ()
showProgressDialog title = do
  showErrorDialog title
  --dialog <- dialogNew
  --set dialog [windowTitle := title]

  --_ <- dialogRun dialog
  --widgetDestroy dialog


catchError :: IO a ->
              IO ()
catchError io = do
  r <- try io
  either (\e -> showErrorDialog $ show (e :: SomeException))
         (\_ -> return ())
         r