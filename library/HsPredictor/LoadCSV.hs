{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module HsPredictor.LoadCSV where
-- standard
import           Control.Exception       (bracket, catch, throwIO)
import           Control.Monad           (when)
import           Control.Monad.Error     (liftIO)
import           Data.Text               (pack)
import           Prelude                 hiding (catch)
import           System.IO               (IOMode (ReadMode), hClose,
                                          hGetContents, openFile)
import           System.IO.Error
-- 3rd party
import           Database.Persist.Sql    (Entity (..), Filter, Key (..),
                                          SelectOpt (LimitTo), SqlBackend,
                                          SqlPersistM, deleteWhere, entityKey,
                                          get, getBy, insert, insertBy,
                                          runMigration, runMigrationSilent,
                                          selectList, selectList, toSqlKey,
                                          update, (+=.), (=.), (==.), (>.))
import           Database.Persist.Sqlite (runSqlite)
-- own
import           HsPredictor.HashCSV     (checkHash, genHash)
import           HsPredictor.Models
import           HsPredictor.ParserCSV   (readMatches)
import           HsPredictor.Queries
import           HsPredictor.Types       (Field (..), Match (..), Result (..))

-- | Inserts match to database
insertMatch :: Match -> SqlPersistM ()
insertMatch (Match {dateM=d,homeM=ht,awayM=at,ghM=gh,gaM=ga,
                    odds1M=o1,oddsxM=ox,odds2M=o2}) = do
  idHome <- getKeyTeams ht
  idAway <- getKeyTeams at
  insert $ Results d idHome idAway gh ga o1 ox o2
  updateTable idHome $ getResult Home gh ga
  updateTable idAway $ getResult Away gh ga

-- | Return sql key for a given team name
getKeyTeams :: String -> SqlPersistM (Key Teams)
getKeyTeams team = do
  eitherKey <- insertBy $ Teams team
  return $ keyFromEither eitherKey

-- | Return result of match
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

-- | Update team statistics
updateTable :: Key Teams -> Result -> SqlPersistM ()
updateTable team result = do
  eitherKey <- insertBy $ StatsTable team 0 0 0
  let teamId = keyFromEither eitherKey
  case result of
   Win -> update teamId [StatsTableWin +=. 1]
   Draw -> update teamId [StatsTableDraw +=. 1]
   Loss -> update teamId [StatsTableLoss +=. 1]
   Upcoming -> return ()

-- | Return sql key form Either
keyFromEither :: Either (Entity record) (Key record) -> Key record
keyFromEither (Left ent) = entityKey ent
keyFromEither (Right key) = key

-- | Insert matches into database. Only if csv was updated.
-- | checkHash prevents from processing the same file twice.
bulkInsert :: [Match] -> String -> String -> SqlPersistM ()
bulkInsert xs hashF hashDB =
  when (checkHash hashF hashDB) $ do
    deleteWhere ([] :: [Filter Results])
    deleteWhere ([] :: [Filter StatsTable])
    deleteWhere ([] :: [Filter Teams])
    mapM_ insertMatch xs

-- | Main function for processing matches and inserting into database.
action :: [Match] -- ^ list of matches
          -> String -- ^ hash of csv file
          -> String -- ^ path to database
          -> IO ()
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

-- | Return file contents
getFileContents :: String -> IO String
getFileContents fname = readF `catch` handleExists
  where
    readF = do
      csvH <- openFile fname ReadMode
      !full <- hGetContents csvH
      hClose csvH
      return full
    handleExists e
      | isDoesNotExistError e = return ""
      | otherwise = throwIO e

-- | Load csv into database
loadCSV :: String -> String -> IO ()
loadCSV fname  dbname = do
  full <- getFileContents fname
  hash <- genHash full
  let matches = readMatches $ lines full
  action matches hash dbname
