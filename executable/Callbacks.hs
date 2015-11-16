module Callbacks where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

import HsPredictor.Render
import HsPredictor.LoadCSV

renderTableCb :: Ref Output -> Ref Input -> Ref Button -> IO ()
renderTableCb out inp _ = do
  csv <- getValue inp
  let db = csv ++ ".db"
  loadCSV csv db
  rTable <- renderTable db 15
  setValue out rTable Nothing
  return ()
