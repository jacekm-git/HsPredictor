module HsPredictor.Parse.CSV where

-- standard
import           Control.Monad                 (replicateM)
import           Control.Monad.Error           (fail, liftIO, throwError)
import           Data.List                     (sort)
-- 3rd party
import           Text.ParserCombinators.Parsec (Parser, char, count, digit, eof,
                                                many, noneOf, parse, try, (<|>))
-- own
import           HsPredictor.Types.Types             (Match (..), ThrowsError)

-- | Parse one line from csv file.
readMatch :: String -> ThrowsError Match
readMatch input = case parse parseCsv "csv" input of
  Left err -> throwError $ show err
  Right val -> return val

-- | Parse list of lines from csv file. Return sorted matches list.
readMatches :: [String] ->  [Match]
readMatches = sort . foldr (\x acc -> case readMatch x of
                                   Left _ -> acc
                                   Right m -> m:acc) []

-- | Parser for csv file.
parseCsv :: Parser Match
parseCsv = do
  date <- parseDate
  homeT <- parseTeam
  awayT <- parseTeam
  (goalsH, goalsA) <- parseGoals
  (odd1, oddx, odd2) <- parseOdds
  eof
  return $ Match date homeT awayT goalsH goalsA odd1 oddx odd2

-- | Parser for date.
parseDate :: Parser Int
parseDate = do
  year <- replicateM 4 digit
  char '.'
  month <- replicateM 2 digit
  char '.'
  day <- replicateM 2 digit
  char ','
  return $ read (year++month++day)

-- | Parser for team.
parseTeam :: Parser String
parseTeam = do
  team <- many (noneOf ",")
  char ','
  return team


-- | Parser for goals.
parseGoals :: Parser (Int, Int)
parseGoals = do
  goalsH <- minusOne <|> many digit
  char ','
  goalsA <- checkField goalsH (many digit)
  char ','
  return (read goalsH :: Int, read goalsA :: Int)

-- | Parser for bookie odds.
parseOdds :: Parser (Double, Double, Double)
parseOdds = do
  odd1 <- minusOne <|> odds
  char ','
  oddx <- checkField odd1 odds
  char ','
  odd2 <- checkField oddx odds
  return (read odd1 :: Double,
          read oddx :: Double,
          read odd2 :: Double)

-- | Parser for "-1"
minusOne :: Parser String
minusOne = do
  sign <- char '-'
  one <- char '1'
  return [sign, one]

-- | Parse one odds value.
odds :: Parser String
odds = do
  int <- many digit
  dot <- char '.'
  rest <- many digit
  return $ int ++ [dot] ++ rest

-- | Check for "-1" field
checkField :: String -> Parser String -> Parser String
checkField field p = if field == "-1"
                     then minusOne
                     else p
