module Parser where

-- standard
import Control.Monad.Error (liftIO, throwError, fail)
import Control.Monad (replicateM)
-- 3rd party
import Text.ParserCombinators.Parsec (parse, Parser, many,
                                      digit, noneOf, char,
                                      count, eof, (<|>), try)
-- own
import Types (Match, ThrowsError)

parseCsv :: Parser Match
parseCsv = do
  year <- replicateM 4 digit
  char '.'
  month <- replicateM 2 digit
  char '.'
  day <- replicateM 2 digit
  char ','
  homeT <- many (noneOf ",")
  char ','
  awayT <- many (noneOf ",")
  char ','
  goalsH <- minusOne <|> many digit
  char ','
  goalsA <- if goalsH == "-1"
           then minusOne
           else many digit
  eof
  return $ (read (year++month++day) :: Int,
            homeT,
            awayT,
            read goalsH :: Int,
            read goalsA :: Int)
  where
    minusOne = do
      sign <- char '-'
      one <- char '1'
      return $  [sign, one]

  
readMatch :: String -> ThrowsError Match
readMatch input = case parse parseCsv "csv" input of
  Left err -> throwError $ show err
  Right val -> return val

readMatches :: [String] ->  [Match]
readMatches xs = foldr (\x acc -> case readMatch x of
                                   Left _ -> acc
                                   Right m -> m:acc) [] xs
