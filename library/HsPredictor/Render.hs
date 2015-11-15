module HsPredictor.Render where

import HsPredictor.Queries
import HsPredictor.Types

listToTable :: [(String, [Int])] -> String
listToTable = foldl convert ""
  where convert acc (s, xs) = acc ++ s ++concatMap ( (" " ++) . show) xs ++ "\n"

renderTable :: DbPath -> IO String
renderTable dbname = do
  t <- getStatsAll dbname
  return $ listToTable t
