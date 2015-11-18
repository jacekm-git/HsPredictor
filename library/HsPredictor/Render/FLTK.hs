module HsPredictor.Render.FLTK where

import HsPredictor.SQL.Queries
import HsPredictor.Types
import HsPredictor.Render.Text (convertList)

getTableData :: DbPath -> IO [[String]]
getTableData dbname = do
  t <- getStatsAll dbname
  let t' = convertList t
  return t'
