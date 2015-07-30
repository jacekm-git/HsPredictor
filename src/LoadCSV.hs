{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE BangPatterns          #-}
module LoadCSV where
-- standard
import System.IO (openFile, hGetContents,
                  hClose, IOMode(ReadMode))
import Data.Text (pack)
import Control.Monad.Error (liftIO)
import Control.Monad (when)
-- 3rd party
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (SqlBackend, Filter, SelectOpt (LimitTo),
                             SqlPersistM, Entity(..), Key(..),
                             insertBy, runMigration,get,
                             entityKey, insert, update,
                             (+=.), (=.), (>.), (==.),
                             getBy, selectList,
                             runMigrationSilent,toSqlKey,
                             deleteWhere, selectList)
-- own
import Parser (readMatches)
import Types (Result (..), Field (..), Match (..))
import Models
import HashCSV (genHash, checkHash)
import Queries

insertMatch :: Match -> SqlPersistM ()
insertMatch (Match {dateM=d,homeM=ht,awayM=at,ghM=gh,gaM=ga,
                    odds1M=o1,oddsxM=ox,odds2M=o2}) = do
  idHome <- getKeyTeams ht
  idAway <- getKeyTeams at
  insert $ Results d idHome idAway gh ga o1 ox o2
  updateTable idHome $ getResult Home gh ga
  updateTable idAway $ getResult Away gh ga

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
bulkInsert xs hashF hashDB =
  when (checkHash hashF hashDB) $ do
    deleteWhere ([] :: [Filter Results])
    deleteWhere ([] :: [Filter StatsTable])
    deleteWhere ([] :: [Filter Teams])
    mapM_ insertMatch xs
    
action :: [Match] -> String -> String -> IO ()
action xs hashF dbname = runSqlite (pack dbname) $ do
  runMigrationSilent migrateAll
  isHashDB <- get (toSqlKey 1 :: MD5Id)
  case isHashDB of
   Nothing -> do
     insertBy $ MD5 hashF
     bulkInsert xs hashF ""
   Just h ->  do
     update (toSqlKey 1 :: MD5Id) [MD5Hash =. hashF]
     bulkInsert xs hashF $ mD5Hash h
  return ()

getFileContents :: String -> IO String
getFileContents fname = do
  csvH <- openFile fname ReadMode
  !full <- hGetContents csvH
  hClose csvH
  return full

loadCSV :: String -> String -> IO ()
loadCSV fname  dbname = do
  full <- getFileContents fname
  hash <- genHash full
  let matches = readMatches $ lines full
  action matches hash dbname




