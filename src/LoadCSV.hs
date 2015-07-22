{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
module LoadCSV where
-- standard
import System.IO (openFile, hGetContents,
                  hClose, IOMode(ReadMode))
import Data.Text (pack)
import Control.Monad.Error (liftIO)
-- 3rd party
--import Database.Persist.Sql
import Database.Persist.Sql  (SqlBackend, Filter, SelectOpt (LimitTo),
                              SqlPersistM, Entity(..), Key(..))
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (insertBy, runMigration,get,
                             entityKey, insert, update,
                             (+=.), (=.), (>.), (==.),
                             getBy, selectList,
                             runMigrationSilent,toSqlKey,
                             deleteWhere, selectList)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
-- own
import Parser (readMatches)
import Types (Result (..), Field (..), Match)
import Models
import HashCSV (genHash, checkHash)

insertMatch :: Match -> SqlPersistM ()
insertMatch (d, ht, at, rh, ra) = do
  idHome <- getKeyTeams ht
  idAway <- getKeyTeams at
  insert $ Results d idHome idAway rh ra
  updateTable idHome $ getResult Home rh ra
  updateTable idAway $ getResult Away rh ra

getKeyTeams :: String -> SqlPersistM (Key Teams)
getKeyTeams team = do
  eitherKey <- insertBy $ Teams team
  return $ keyFromEither eitherKey

getResult :: Field -> Int -> Int -> Result
getResult Home rh ra
  | rh == (-1) = Upcoming
  | rh == ra = Draw
  | rh > ra = Win
  | otherwise = Loss
getResult Away rh ra
  | rh == (-1) = Upcoming
  | rh == ra = Draw
  | rh > ra = Loss
  | otherwise = Win
                
updateTable :: Key Teams -> Result -> SqlPersistM ()
updateTable team result = do
  eitherKey <- insertBy $ StatsTable team 0 0 0
  let teamId = keyFromEither eitherKey
  case result of
   Win -> update teamId [StatsTableWin +=. 1]
   Draw -> update teamId [StatsTableDraw +=. 1]
   Loss -> update teamId [StatsTableLoss +=. 1]
   Upcoming -> return ()

keyFromEither :: Either (Entity record) (Key record) -> Key record
keyFromEither (Left ent) = entityKey ent
keyFromEither (Right key) = key

bulkInsert :: [Match] -> String -> String -> SqlPersistM ()
bulkInsert xs hashF hashDB = do
  case checkHash hashF hashDB of
    True -> do
      liftIO $ print "Dropping tables..."
      deleteWhere ([] :: [Filter Results])
      deleteWhere ([] :: [Filter StatsTable])
      deleteWhere ([] :: [Filter Teams])
      liftIO $ print "Inserting csv into tables."
      mapM_ insertMatch xs
    False -> do
      liftIO $ print "Nothing to do. Md5sums match."
      return ()
    
action :: [Match] -> String -> String -> IO ()
action xs hashF dbname = runSqlite (pack dbname) $ do
  runMigrationSilent migrateAll
  isHashDB <- get $ (toSqlKey 1 :: MD5Id)
  case isHashDB of
   Nothing -> do
     liftIO $ print "New file md5sum..."
     insertBy $ MD5 hashF
     bulkInsert xs hashF ""
   Just h ->  do
     liftIO $ print "Comparing Hashes"
     update (toSqlKey 1 :: MD5Id) [MD5Hash =. hashF]
     bulkInsert xs hashF $ mD5Hash h
  return ()

loadCSV :: String -> String -> IO ()
loadCSV fname  dbname = do
  csvH <- openFile fname ReadMode
  full <- hGetContents csvH
  hash <- genHash $ full
  let matches = readMatches $ lines full
  action matches hash dbname
  hClose csvH

-- queries
getTeams :: String -> IO [String]
getTeams dbname = runSqlite (pack dbname) $ do
  teams <- selectList ([] :: [Filter Teams]) []
  return $ map (\x -> teamsName . entityVal $ x) teams

getStats :: String -> String -> IO [Int]
getStats team dbname = runSqlite (pack dbname) $ do
  teamId <- selectList [TeamsName ==. team] [LimitTo 1]
  case teamId of
   [] -> return []
   (x:[]) -> do
     stats <- selectList [StatsTableTeam ==. (entityKey x)] [LimitTo 1]
     return $ [getStat statsTableWin stats ,
               getStat statsTableDraw stats,
               getStat statsTableLoss stats]
     where
       getStat f = f . entityVal . head

qResultsAll dbname = runSqlite (pack dbname)
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

qResultsUpcoming dbname = runSqlite (pack dbname)
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


getResultsAll dbname = do
  r <- qResultsAll dbname
  return r

getResultsUpcoming dbname = do
  r <- qResultsUpcoming dbname
  return r  
