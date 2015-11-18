{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module HsPredictor.SQL.Models.League where

-- 3rd party
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
-- | Template haskell: database model.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Teams
  name String
  Name name
  deriving Show
Results
  date Int
  homeTeam TeamsId
  awayTeam TeamsId
  resultHome Int
  resultAway Int
  odds1x Double
  oddsx Double
  oddsx2 Double
  deriving Show
StatsTable
  team TeamsId
  win Int
  draw Int
  loss Int
  Team team
  deriving Show
MD5
  hash String
  Hash hash
  deriving Show
|]
