{-# Language BangPatterns #-}
module ExportCSV where

-- standard
import System.IO (appendFile)
import Data.Text (pack)
-- 3rd party
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql
-- own
import LoadCSV (insertMatch, getFileContents)
import Parser (readMatches)
import Models
import Queries
import Types


normalize val min max =  x / y - 1
  where
    x = fromIntegral $ 2*(val - min) :: Double
    y = case max - min of
         0 -> 1 :: Double
         v -> fromIntegral $ v :: Double

outcome :: Int -> Int -> Outcome
outcome x y
  | x > y = HomeWin
  | x < y = AwayWin
  | otherwise = NoWinner

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
getScaledStats :: String -> String -> IO Scaled
getScaledStats dbname team = do
  w <- getScaledWin dbname team
  d <- getScaledDraw dbname team
  l <- getScaledLoss dbname team
  return $ Scaled w d l

--getScaledStat ::  String -> String -> IO Double
getScaledStat stat dbname team = do
  w <- getStat dbname team stat
  max_w <- getMaxStat dbname stat
  min_w <- getMinStat dbname stat
  if w < min_w
    then return $ (-1)
    else return $ normalize w min_w max_w
 
getScaledWin :: String -> String -> IO Double
getScaledWin = getScaledStat StatsTableWin

getScaledDraw :: String -> String -> IO Double
getScaledDraw = getScaledStat StatsTableDraw

getScaledLoss :: String -> String -> IO Double
getScaledLoss = getScaledStat StatsTableLoss


-- third step
-- write to file
prepareLine :: String -> String -> String -> Outcome -> IO String
prepareLine dbpath home away out = do
  h <- getScaledStats dbpath home
  a <- getScaledStats dbpath away
  return $ show h ++ show a ++ "\n" ++ show out ++ "\n"


writeExport :: String -> IO String -> IO ()
writeExport fpath x = do
  line <- x
  print line
  appendFile fpath line
  
processRound :: String -> String -> [Match] -> IO ()
processRound dbPath fpath m = do
  runSqlite (pack dbPath) $ do
    runMigrationSilent migrateAll
  let matches = filter (\x -> ghM x >= 0) m
  let lines = map prepare matches
  mapM_ (writeExport fpath) lines
  insertRound m dbPath
  where
    prepare x = let home = homeM x
                    away = awayM x
                    out = outcome (ghM x) (gaM x)
                in prepareLine dbPath home away out
                   

insertRound :: [Match] -> String -> IO ()
insertRound xs dbPath = runSqlite (pack dbPath) $ do
  runMigrationSilent migrateAll
  mapM_ insertMatch xs
  
-- last step
export :: String -> String -> String -> IO ()
export dbPath expPath csvPath = do
  ms <- getFileContents csvPath
  let matches = readMatches $ lines ms
  let rounds = genListsByDate matches
  mapM_ (processRound dbPath expPath) rounds
  return ()
