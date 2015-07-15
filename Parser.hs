module Parser where

-- standard
import Control.Monad.Error (liftIO, throwError)

-- 3rd party
import Text.ParserCombinators.Parsec (parse, Parser, many,
                                      digit, noneOf, char)
-- own
import Types (Match, ThrowsError)

parseCsv :: Parser Match
parseCsv = do
  year <- many digit
  char '.'
  month <- many digit
  char '.'
  day <- many digit
  char ','
  homeT <- many (noneOf ",")
  char ','
  awayT <- many (noneOf ",")
  char ','
  goalsH <- many digit
  char ','
  goalsA <- many digit
  return $ (read (year++month++day) :: Int,
            homeT,
            awayT,
            read goalsH :: Int,
            read goalsA :: Int)
  
readMatch :: String -> ThrowsError Match
readMatch input = case parse parseCsv "csv" input of
  Left err -> throwError $ show err
  Right val -> return val

readMatches :: [String] ->  [Match]
readMatches xs = foldr (\x acc -> case readMatch x of
                                   Left _ -> acc
                                   Right m -> m:acc) [] xs
