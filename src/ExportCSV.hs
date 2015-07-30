module ExportCSV where

-- standard
import Data.Text (pack)
-- 3rd party
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist.Sqlite (runSqlite)
-- own
import LoadCSV (getFileContents, insertMatch, updateTable)
import Models
import Queries
import Types



normalize val min max =  x / y - 1
  where
    x = fromIntegral $ 2*(val - min) :: Double
    y = case max - min of
         0 -> 1 :: Double
         v -> fromIntegral $ v :: Double

-- first step -- list must be sorted
genListsByDate :: [Match] -> [[Match]]
genListsByDate [] = []
genListsByDate (x:xs) = [x:(takeWhile compareMatches xs)] ++
                        (genListsByDate $ dropWhile compareMatches xs)
  where
    compareMatches y = case x `compare` y of
                      EQ -> True
                      otherwise -> False

-- second step
-- get scaled stats
getScaledStats :: String -> IO Scaled
getScaledStats = undefined


--getScaledStat :: String -> String -> IO Double
getScaledStat stat dbname team = do
  w <- getStat dbname team stat
  (Just max_w) <- getMaxStat dbname team stat
  (Just min_w) <- getMinStat dbname team stat
  return $ normalize w min_w max_w

getScaledWin :: String -> String -> IO Double
getScaledWin = getScaledStat StatsTableWin

getScaledDraw :: String -> String -> IO Double
getScaledDraw = getScaledStat StatsTableDraw

getScaledLoss :: String -> String -> IO Double
getScaledLoss = getScaledStat StatsTableLoss


-- third step
-- write to file
appendToExported :: String -> Scaled -> Scaled -> Double -> IO ()
appendToExported = undefined

-- last step
bulkInsert :: [Match] -> IO ()
bulkInsert = undefined
