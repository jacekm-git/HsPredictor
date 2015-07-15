{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Models where

-- 3rd party
import Database.Persist.TH (share, mkPersist, sqlSettings,
                            mkMigrate, persistLowerCase)

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
  deriving Show
StatsTable
  team TeamsId
  win Int
  draw Int
  loss Int
  Team team
  deriving Show
|]
