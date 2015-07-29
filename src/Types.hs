module Types where

type ThrowsError = Either String
data Match = Match {
  dateM :: Int,
  homeM :: String,
  awayM :: String,
  ghM :: Int,
  gaM :: Int,
  odds1M :: Double,
  oddsxM :: Double,
  odds2M :: Double} deriving (Show, Eq)
                            
data Result = Win | Draw | Loss | Upcoming
data Field = Home | Away

data Scaled = Scaled {
  winS :: Double,
  drawS :: Double,
  loseS :: Double} deriving (Show)
