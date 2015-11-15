module HsPredictor.RawSQL where

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
import           HsPredictor.Types


getUpcomingQuery, getResultsAllQuery :: DbPath ->IO [(E.Value Int,
                                                 E.Value String,
                                                 E.Value String,
                                                 E.Value Int,
                                                 E.Value Int)]
getResultsAllQuery dbname = runSqlite (pack dbname)
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

getUpcomingQuery dbname = runSqlite (pack dbname)
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


getTeamsQuery dbname = runSqlite (pack dbname) $
                       selectList ([] :: [Filter Teams]) []

getStatsQuery dbname team = runSqlite (pack dbname) $ do
  teamId <- selectList [TeamsName ==. team] [LimitTo 1]
  case teamId of
   [] -> return []
   [x] -> selectList [StatsTableTeam ==. entityKey x] [LimitTo 1]

getStatQuery dbname team stat = runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \(t1 `E.InnerJoin` t2) -> do
                        E.on $ t1 ^. StatsTableTeam E.==. t2 ^. TeamsId
                        E.where_ (t2 ^. TeamsName E.==. E.val team)
                        return $ t1 ^. stat


getMaxStatQuery dbname stat = runSqlite (pack dbname)
                              $ E.select
                              $ E.from $ \t -> return $ E.max_ $ t ^. stat


getMinStatQuery dbname stat = runSqlite (pack dbname)
                      $ E.select
                      $ E.from $ \t -> return $ E.min_ $ t ^. stat


getStatsAllQuery :: DbPath -> IO [(E.Value String, Entity StatsTable)]
getStatsAllQuery dbname = runSqlite (pack dbname)
              $ E.select
              $ E.from $ \(t `E.InnerJoin` s) -> do
                E.on $ s ^. StatsTableTeam E.==. t ^. TeamsId
                E.orderBy [E.desc (s ^. StatsTableDraw E.+.
                                   s ^. StatsTableWin E.*. E.val 3)]
                return (t ^. TeamsName, s)
