module HsPredictor.Queries where

--standard
import           Data.Text               (pack)
--3rd party
import           Database.Esqueleto      ((^.))
import qualified Database.Esqueleto      as E
import           Database.Persist.Sql    (Entity (..), Filter, Key (..),
                                          SelectOpt (LimitTo), selectList,
                                          toSqlKey, (==.), (>.) )
import           Database.Persist.Sqlite (runSqlite)
--own
import           HsPredictor.Models

-- | Unpack from Esqueleto Value
unVal :: E.Value a -> a
unVal (E.Value a) = a

-- | Unpack head from Esquelto Value list.
headUnVal ::  (Num s) => IO [E.Value s] -> IO s
headUnVal val = do
  v <- val
  case v of
   [] -> return 0
   _ -> return $ unVal . head $ v


-- | Unpack head from Esqueleto Value (Maybe a) list
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
-- ^ Return all results form database
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

-- ^ Return upcoming matches from database.
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

-- | Return list of teams
getTeams :: String -> IO [String]
getTeams dbname = runSqlite (pack dbname) $ do
  teams <- selectList ([] :: [Filter Teams]) []
  return $ map (\x -> teamsName . entityVal $ x) teams


-- | Return stats of given team
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

-- | Return given stat of a given team
getStat :: (E.PersistField a, Num a) =>
           String -- ^path to Database
           -> String -- ^team name
           -> EntityField StatsTable a -- ^ StatsTable column
           -> IO a
getStat dbname team stat = headUnVal $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \(t1 `E.InnerJoin` t2) -> do
                        E.on $ t1 ^. StatsTableTeam E.==. t2 ^. TeamsId
                        E.where_ (t2 ^. TeamsName E.==. E.val team)
                        return $ t1 ^. stat

-- | Return max value of given stat
getMaxStat :: (E.PersistField a, Num a) =>
              String -- ^ path to Database
              -> EntityField StatsTable a -- ^ StatsTable column
              -> IO a
getMaxStat dbname stat = headUnVal' $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \t -> return $ E.max_ $ t ^. stat


-- | Return min value of given stat
getMinStat :: (E.PersistField a, Num a) =>
              String -- ^ path to Database
              -> EntityField StatsTable a -- ^ StatsTable column
              -> IO a
getMinStat dbname stat = headUnVal' $ runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \t -> return $ E.min_ $ t ^. stat


getStatsAllQuery :: String -> IO [(E.Value String, Entity StatsTable)]
getStatsAllQuery dbname = runSqlite (pack dbname)
              $ E.select
              $ E.from $ \(t `E.InnerJoin` s) -> do
                E.on $ s ^. StatsTableTeam E.==. t ^. TeamsId
                E.orderBy [E.desc (s ^. StatsTableDraw E.+.
                                   s ^. StatsTableWin E.*. E.val 3)]
                return (t ^. TeamsName, s)

getStatsAll :: String -> IO [(String, [Int])]
getStatsAll dbname = do
  stats <- getStatsAllQuery dbname
  return $ map extract stats
  where
    extract (team, st) = (unVal team, [ statsTableWin . entityVal $  st
                                      , statsTableDraw . entityVal $  st
                                      , statsTableLoss . entityVal $  st])
