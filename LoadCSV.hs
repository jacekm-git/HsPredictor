{-# LANGUAGE OverloadedStrings          #-}
module LoadCSV where
-- standard
import System.IO (openFile, hGetContents,
                  hClose, IOMode(ReadMode))
import Control.Monad.Error (liftIO)

-- 3rd party
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (insertBy, runMigration,
                             entityKey, insert, update,
                             (+=.), (=.), (>.), getBy, selectList)
-- own
import Parser (readMatches)
import Types (Result (..), Field (..), Match)
import Models

insertMatch (d, ht, at, rh, ra) = do
  runMigration migrateAll
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

bulkInsert xs = mapM_ insertMatch xs

action :: [Match] -> IO ()
action xs = runSqlite "tests.db" $ do
  bulkInsert xs
  return ()

loadCSV :: String -> IO ()
loadCSV f = do
  csvH <- openFile f ReadMode
  full <- hGetContents csvH
  let matches = readMatches $ lines full
  action matches
  hClose csvH
  putStrLn "CSV loaded into SQL tables"
