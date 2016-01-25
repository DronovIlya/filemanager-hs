-- |This module provides user interaction with popup dialogs
module GUI.Popup where

import Control.Exception ( catch, throw, try, SomeException )
import Graphics.UI.Gtk

-- | Error dialog in case of any exceptions
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

-- | Try and catch error. In case of catched exception, show
-- error dialog
catchError :: IO a ->
              IO ()
catchError io = do
  r <- try io
  either (\e -> showErrorDialog $ show (e :: SomeException))
         (\_ -> return ())
         r

-- | Text input dialog.
textInputDialog :: String -> 
                   IO (Maybe String)
textInputDialog title = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    title
  entry <- entryNew
  cbox <- dialogGetActionArea chooserDialog
  dialogAddButton chooserDialog "Ok"     (ResponseUser 0)
  dialogAddButton chooserDialog "Cancel" (ResponseUser 1)
  boxPackStart (castToBox cbox) entry PackNatural 5
  widgetShowAll chooserDialog
  rID <- dialogRun chooserDialog
  ret <- case rID of
           ResponseUser 0 -> Just <$> entryGetText entry
           ResponseUser 1 -> return Nothing
  widgetDestroy chooserDialog
  return ret
