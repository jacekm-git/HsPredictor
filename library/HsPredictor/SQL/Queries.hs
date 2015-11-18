module HsPredictor.SQL.Queries where

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
import           HsPredictor.SQL.Models.League
import           HsPredictor.Types.Types
import           HsPredictor.SQL.Raw


unVal :: E.Value a -> a
unVal (E.Value a) = a

headUnVal :: Num a => [E.Value a] -> a
headUnVal v = case v of
  [] -> 0
  (x:xs) -> unVal x

unValMaybe ::  (Num s) => E.Value (Maybe s) -> s
unValMaybe val = case val of
  E.Value (Just a) -> a
  E.Value Nothing -> 0


getResultsAll dbname = do
  r <- getResultsAllQuery dbname
  return $ map extract r
  where
    extract (d, t1, t2, gh, ga) = (unVal d, unVal t1,
                                   unVal t2, unVal gh, unVal ga)

getUpcoming dbname = do
  u <- getUpcomingQuery dbname
  return $ map extract u
  where
    extract (d, t1, t2, gh, ga) = (unVal d, unVal t1,
                                   unVal t2, unVal gh, unVal ga)

getTeams dbname = do
  t <- getTeamsQuery dbname
  return $ map (\x -> teamsName . entityVal $ x) t


getStats dbname team = do
  st <- getStatsQuery team dbname
  return $ extract st
  where
    getStat f = f . entityVal . head
    extract st = [getStat statsTableWin st,
                  getStat statsTableDraw st,
                  getStat statsTableLoss st]


getStat dbname team stat = do
  st <- getStatQuery dbname team stat
  return $ headUnVal st


getMaxStat dbname stat = do
  st <- getMaxStatQuery dbname stat
  return $ unValMaybe . head $ st


getMinStat dbname stat = do
  st <- getMinStatQuery dbname stat
  return $ unValMaybe . head  $ st


getStatsAll :: DbPath -> IO [(String, [Int])]
getStatsAll dbname = do
  stats <- getStatsAllQuery dbname
  return $ map extract stats
  where
    extract (team, st) = (unVal team, [ statsTableWin . entityVal $  st
                                      , statsTableDraw . entityVal $  st
                                      , statsTableLoss . entityVal $  st])
