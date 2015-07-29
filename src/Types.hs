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

instance Ord Match where
  m1 `compare` m2 = (dateM m1) `compare` (dateM m2)
                            
data Result = Win | Draw | Loss | Upcoming
data Field = Home | Away

data Scaled = Scaled {
  winS :: Double,
  drawS :: Double,
  loseS :: Double} deriving (Show)
