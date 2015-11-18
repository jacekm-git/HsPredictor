module HsPredictor.Render.Text where

import HsPredictor.SQL.Queries
import HsPredictor.Types.Types


convertList :: [(String, [Int])] -> [[String]]
convertList = map convert
  where convert (s, xs) = s : map show xs

addPadding xs width = map convert xs
  where
    convert xs = concatMap  mkPad xs ++ "|"
    mkPad s = let sL = length s
                  pad = "|" ++ replicate (width -sL) ' '
              in pad ++ s

addInterlines w [] = []
addInterlines w (x:xs) = x : replicate (length x) '-': addInterlines w xs

renderList xs width = addInterlines width $ addPadding xs width

renderTable dbname width = do
  t <- getStatsAll dbname
  let t' = convertList t
  return $ unlines $ renderList t' width
