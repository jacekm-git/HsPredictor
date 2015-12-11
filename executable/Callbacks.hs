module Callbacks where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.IORef
import HsPredictor.Render.FLTK
import HsPredictor.CSV.Load


btnCallback :: IORef [[String]] -> Ref Table
               -> Ref Input -> Ref Button -> IO ()
btnCallback tableData table inp _ = do
  csv <- getValue inp
  let db = csv ++ ".db"
  loadCSV csv db
  newtData <- getTableData db
  writeIORef tableData newtData
  redraw table
