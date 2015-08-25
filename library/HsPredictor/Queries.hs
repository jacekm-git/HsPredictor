module HsPredictor.Queries where

--standard
import Data.Text (pack)
--3rd party
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (Filter, SelectOpt (LimitTo),
                             Entity(..), Key(..),
                             (>.), (==.), selectList,toSqlKey)
--own
import HsPredictor.Models


-- unpack from Esquelto Value
unVal :: E.Value a -> a
unVal (E.Value a) = a

headUnVal ::  (Num s) => IO [E.Value s] -> IO s
headUnVal val = do
  v <- val
  case v of
   [] -> return 0
   _ -> return $ unVal . head $ v

headUnVal' ::  (Num s) => IO [E.Value (Maybe s)] -> IO s
headUnVal' val = do
  v <- val
  case v of
   [] -> return 0
   [E.Value (Just a)] -> return a
   [E.Value Nothing] -> return 0
   

getResultsUpcoming, getResultsAll :: String ->IO [(E.Value Int,
                                                 E.Value String,
                                                 E.Value String,
                                                 E.Value Int,
                                                 E.Value Int)]
getResultsAll dbname = runSqlite (pack dbname)
              $ E.select
              $ E.from $ \(t1 `E.InnerJoin` r `E.InnerJoin` t2) -> do
                E.on $ r ^. ResultsHomeTeam E.==. t1 ^. TeamsId
                E.on $ r ^. ResultsAwayTeam E.==. t2 ^. TeamsId
                return (
                  r ^. ResultsDate,
                  t1 ^. TeamsName,
                  t2 ^. TeamsName,
                  r ^. ResultsResultHome,
                  r ^. ResultsResultAway)
                  

getResultsUpcoming dbname = runSqlite (pack dbname)
              $ E.select
              $ E.from $ \(t1 `E.InnerJoin` r `E.InnerJoin` t2) -> do
                E.on $ r ^. ResultsHomeTeam E.==. t1 ^. TeamsId
                E.on $ r ^. ResultsAwayTeam E.==. t2 ^. TeamsId
                E.where_ (r ^. ResultsResultHome E.==. E.val (-1))
                return (
                  r ^. ResultsDate,
                  t1 ^. TeamsName,
                  t2 ^. TeamsName,
                  r ^. ResultsResultHome,
                  r ^. ResultsResultAway)

getTeams :: String -> IO [String]
getTeams dbname = runSqlite (pack dbname) $ do
  teams <- selectList ([] :: [Filter Teams]) []
  return $ map (\x -> teamsName . entityVal $ x) teams

getStats :: String -> String -> IO [Int]
getStats team dbname = runSqlite (pack dbname) $ do
  teamId <- selectList [TeamsName ==. team] [LimitTo 1]
  case teamId of
   [] -> return []
   [x] -> do
     stats <- selectList [StatsTableTeam ==. entityKey x] [LimitTo 1]
     return [getStat statsTableWin stats ,
             getStat statsTableDraw stats,
             getStat statsTableLoss stats]
     where
       getStat f = f . entityVal . head                  

--getStat :: String -> String -> IO Int
getStat dbname team stat = headUnVal $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \(t1 `E.InnerJoin` t2) -> do
                        E.on $ t1 ^. StatsTableTeam E.==. t2 ^. TeamsId
                        E.where_ (t2 ^. TeamsName E.==. E.val team)
                        return $ t1 ^. stat

--getMaxWins :: String -> String -> IO [E.Value Int]
getMaxStat dbname stat = headUnVal' $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \t -> return $ E.max_ $ t ^. stat

--getMinWins :: String -> String -> IO [E.Value Int]
getMinStat dbname stat = headUnVal' $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \t -> return $ E.min_ $ t ^. stat
