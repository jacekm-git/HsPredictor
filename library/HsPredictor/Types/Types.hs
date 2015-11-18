module HsPredictor.Types.Types where

type ThrowsError = Either String

type DbPath = String
type TeamName = String

-- | Match - contains all parsed values
data Match = Match {
  dateM  :: Int,
  homeM  :: String,
  awayM  :: String,
  ghM    :: Int,
  gaM    :: Int,
  odds1M :: Double,
  oddsxM :: Double,
  odds2M :: Double} deriving (Show, Eq)


-- | Sorting matches by date
instance Ord Match where
  m1 `compare` m2 = dateM m1 `compare` dateM m2

data Result = Win | Draw | Loss | Upcoming
data Outcome = HomeWin | NoWinner | AwayWin

instance Show Outcome where
  show HomeWin = "-1"
  show NoWinner = "0"
  show AwayWin = "1"

data Field = Home | Away

-- | Used for export
data Scaled = Scaled {
  winS  :: Double,
  drawS :: Double,
  loseS :: Double} deriving (Eq)

instance Show Scaled where
  show (Scaled w d l) = show w ++ " " ++ show d ++ " " ++ show l ++ " "
