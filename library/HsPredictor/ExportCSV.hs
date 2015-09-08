module HsPredictor.ExportCSV where

-- standard
import           Control.Monad           (liftM)
import           Data.Text               (pack)
import           System.IO               (appendFile)
-- 3rd party
import           Database.Esqueleto      ((^.))
import qualified Database.Esqueleto      as E
import           Database.Persist.Sql
import           Database.Persist.Sqlite (runSqlite)
-- own
import           HsPredictor.LoadCSV     (getFileContents, insertMatch)
import           HsPredictor.Models
import           HsPredictor.ParserCSV   (readMatches)
import           HsPredictor.Queries
import           HsPredictor.Types

{-| Scale value to range [-1,1] -}
normalize :: Int -- ^ current value
          -> Int -- ^ min value
          -> Int -- ^ max value
          -> Double
normalize val min max =  x / y - 1
  where
    x = fromIntegral $ 2*(val - min) :: Double
    y = case max - min of
         0 -> 1 :: Double
         v -> fromIntegral v :: Double
-- | Based on goals scored by each team return outcome of match
outcome :: Int -- ^ goals scored by home team
        -> Int -- ^ goals scored by away team
        -> Outcome
outcome x y
  | x > y = HomeWin
  | x < y = AwayWin
  | otherwise = NoWinner

{-| Split list of matches by date.
Group matches with the same date in a separate lists.
-}
genListsByDate :: [Match] -- ^ list of matches (must be sorted)
               -> [[Match]] -- ^ matches grouped by date
genListsByDate [] = []
genListsByDate (x:xs) = (x:takeWhile compareMatches xs):
                        genListsByDate (dropWhile compareMatches xs)
  where
    compareMatches y = case x `compare` y of
                      EQ -> True
                      otherwise -> False


{-| Return team stats, every stat value scaled to range [-1,1] -}
getScaledStats :: String -- ^ name of database file
               -> String -- ^ name of a team
               -> IO Scaled
getScaledStats dbname team = do
  w <- getScaledWin dbname team
  d <- getScaledDraw dbname team
  l <- getScaledLoss dbname team
  return $ Scaled w d l

{-| Return given stat scaled to [-1,1] -}
getScaledStat :: EntityField StatsTable Int  -- ^ StatsTable column
              -> String -- ^ database name
              -> String -- ^ team name
              -> IO Double
getScaledStat stat dbname team = do
  w <- getStat dbname team stat
  max_w <- getMaxStat dbname stat
  min_w <- getMinStat dbname stat
  if w < min_w
    then return (-1)
    else return $ normalize w min_w max_w

getScaledWin :: String -> String -> IO Double
getScaledWin = getScaledStat StatsTableWin

getScaledDraw :: String -> String -> IO Double
getScaledDraw = getScaledStat StatsTableDraw

getScaledLoss :: String -> String -> IO Double
getScaledLoss = getScaledStat StatsTableLoss


{-| Returns line ready to write to export file -}
prepareLine :: String -- ^ path to Database
            -> String -- ^ home team name
            -> String -- ^ away team name
            -> Outcome -> IO String
prepareLine dbpath home away out = do
  h <- getScaledStats dbpath home
  a <- getScaledStats dbpath away
  return $ show h ++ show a ++ "\n" ++ show out ++ "\n"


{-| Writes line to export file -}
writeExport :: String -- ^ path to export file
            -> IO String -- ^ prepared line
            -> IO ()
writeExport fpath x = do
  line <- x
  appendFile fpath line


{-| Insert matches to database. Write data to export file -}
processRound :: String -- ^ path to database
             -> String -- ^ path to export file
             -> [Match] -- ^ list of matches with the same date
             -> IO ()
processRound dbPath fpath m = do
  runSqlite (pack dbPath) $ runMigrationSilent migrateAll
  let matches = filter (\x -> ghM x >= 0) m
  let lines = map prepare matches
  mapM_ (writeExport fpath) lines
  insertRound m dbPath
  where
    prepare x = let home = homeM x
                    away = awayM x
                    out = outcome (ghM x) (gaM x)
                in prepareLine dbPath home away out


{-| Insert matches to db -}
insertRound :: [Match] -> String -> IO ()
insertRound xs dbPath = runSqlite (pack dbPath) $ do
  runMigrationSilent migrateAll
  mapM_ insertMatch xs

{-| Add header to export file. Number of exported
matches, number of input neurons, number of output neurons -}
addHeader :: String -- ^ path to export file
             -> IO ()
addHeader path = do
  f <- lines `liftM` getFileContents path
  let input = show . length . words . head $ f
  let output = show . length . words $ f !! 1
  let matches = show $ length f `div` 2
  let header = matches ++ " " ++ input ++ " " ++ output ++ "\n"
  let new = header ++ (unlines f)
  writeFile path new

{-| Insert CSV file to database and write data to export file -}
export :: String -- ^ path to database
       -> String -- ^ path to export file
       -> String -- ^ path to csv file
       -> IO ()
export dbPath expPath csvPath = do
  ms <- getFileContents csvPath
  let matches = readMatches $ lines ms
  let rounds = genListsByDate matches
  mapM_ (processRound dbPath expPath) rounds
  addHeader expPath
  return ()
