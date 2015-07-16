{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
module LoadCSV where
-- standard
import Data.Int(Int64)
import System.IO (openFile, hGetContents,
                  hClose, IOMode(ReadMode))
import Control.Monad.Error (liftIO)
import Control.Monad.Trans.Reader (ReaderT)

-- 3rd party
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (insertBy, runMigration,get,
                             entityKey, insert, update,
                             (+=.), (=.), (>.), (==.),
                             getBy, selectList)
import Database.Persist.Types (PersistValue(PersistInt64))

-- own
import Parser (readMatches)
import Types (Result (..), Field (..), Match)
import Models
import HashCSV (genHash, checkHash)

insertMatch (d, ht, at, rh, ra) = do
  idHome <- getKeyTeams ht
  idAway <- getKeyTeams at
  insert $ Results d idHome idAway rh ra
  updateTable idHome $ getResult Home rh ra
  updateTable idAway $ getResult Away rh ra

getKeyTeams team = do
  eitherKey <- insertBy $ Teams team
  return $ keyFromEither eitherKey

getResult :: Field -> Int -> Int -> Result
getResult Home rh ra
  | rh == ra = Draw
  | rh > ra = Win
  | otherwise = Loss
getResult Away rh ra
  | rh == ra = Draw
  | rh > ra = Loss
  | otherwise = Win

updateTable team result = do
  eitherKey <- insertBy $ StatsTable team 0 0 0
  let teamId = keyFromEither eitherKey
  case result of
   Win -> update teamId [StatsTableWin +=. 1]
   Draw -> update teamId [StatsTableDraw +=. 1]
   Loss -> update teamId [StatsTableLoss +=. 1]


keyFromEither (Left ent) = entityKey ent
keyFromEither (Right key) = key

intToKey :: Integer -> MD5Id
intToKey i = toSqlKey (fromIntegral i)

bulkInsert xs hashF hashDB =
  case checkHash hashF hashDB of
    True -> (liftIO $ print "work") >>
           mapM_ insertMatch xs
    False -> (liftIO $ print "not!") >> return ()
action :: [Match] -> String -> IO ()
action xs hashF = runSqlite "tests.db" $ do
  runMigrationSilent migrateAll
  isHashDB <- get $ intToKey 1 
  case isHashDB of
   Nothing -> do
     insertBy $ MD5 hashF
     bulkInsert xs hashF ""
   Just h ->  do
     bulkInsert xs hashF $ mD5Hash h
  return ()

loadCSV :: String -> IO ()
loadCSV f = do
  csvH <- openFile f ReadMode
  full <- hGetContents csvH
  hash <- genHash $ full
  let matches = readMatches $ lines full
  action matches hash
  hClose csvH
  putStrLn "CSV loaded into SQL tables"

