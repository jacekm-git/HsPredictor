{-# LANGUAGE RankNTypes #-}
module HsPredictor.Parse.HTML where
import           Control.Monad                 (replicateM)
import           Data.Char                     (isDigit)
import           Data.List                     (intercalate, sort)
import           Data.Text                     (pack, strip, unpack)
--3rd party
import           Text.HandsomeSoup
import           Text.Parsec.Error             (ParseError)
import           Text.ParserCombinators.Parsec (Parser, char, count, digit, eof,
                                                many, noneOf, parse, space, try,
                                                (<|>))
import           Text.XML.HXT.Arrow.XmlArrow   (ArrowXml)
import           Text.XML.HXT.Core
import           Text.XML.HXT.DOM.TypeDefs     (XmlTree)
--own
import           HsPredictor.Types.Types

-- | Strip whitespaces around string
strip' :: String -> String
strip' s = unpack . strip . pack $ s

-- | Parser for results page on betexploer
fromResultsHTML :: ArrowXml a => a XmlTree XmlTree -> a XmlTree [String]
fromResultsHTML doc = doc //> css "table tr" >>>
               listA (date <+> teamsOrResult <+> odds)
  where
    odds = css ".odds" >>> getAttrValue "data-odd"
    teamsOrResult = css "td a" /> getText
    date = css ".date" /> getText


-- | Parser for fixtures page on betexploer
fromFixturesHTML :: ArrowXml a => a XmlTree XmlTree -> a XmlTree [String]
fromFixturesHTML doc = doc //> css "tr.match-line" >>>
                       listA (date <+> teams <+> odds)
  where
    date = css ".date" >>> getChildren >>> getText
    teams = css ".tl" >>> getChildren >>> getChildren >>> getText
    odds =  css ".mySelectionsTip" >>> getAttrValue "data-odd"

type ConvertFunction = ArrowXml a => a XmlTree XmlTree -> a XmlTree [String]
-- | Convert html file to list easily parseable list of strings
convertHtmlFile :: String -> ConvertFunction -> IO [String]
convertHtmlFile path func = do
  html <- readFile path
  convertHtmlString html func
-- | Convert contents of html page to easily parseable list of strings
convertHtmlString :: String -> ConvertFunction -> IO [String]
convertHtmlString str func = do
  m <- runX $ func $ readString [withParseHTML yes, withWarnings no] str
  return $ fillHoles (clean m) ""
  where clean xs =  filter (/= "") $
                    map (intercalate ", " . filter (/="\16")) xs

-- | After conversion from html some matches doesn't have date assigned.
-- | This function fix this  problem by assigning correct dates.
fillHoles :: [String] -> String -> [String]
fillHoles [] _ = []
fillHoles (x:xs) d = if all isDigit $ take 2 x
                     then x: fillHoles xs (take 10 x)
                     else (d ++ "," ++ x): fillHoles xs d

-- | Parse one line
readMatch :: String -> Either ParseError String
readMatch = parse (try parseHTMLResults <|> parseHTMLFixtures) ""

-- | Parse list of string lines
readMatches :: [String] ->  [String]
readMatches = sort . foldr (\x acc -> case readMatch x of
                                   Left _ -> acc
                                   Right m -> m:acc) []

-- | Parser for results page
parseHTMLResults :: Parser String
parseHTMLResults = do
  date <- parseDate
  (tH, tA) <- parseTeams
  (gH, gA) <- parseGoals
  (o1, ox, o2) <- try parseOdds <|> return ("-1.0","-1.0","-1.0")
  let  line = intercalate "," [date,tH,tA,gH,gA,o1,ox,o2] ++ " \n"
  return line

-- | Parser for fixtures page
parseHTMLFixtures :: Parser String
parseHTMLFixtures = do
  date <- parseDate
  (tH, tA) <- parseTeams
  (o1, ox, o2) <- parseOdds  <|> return ("-1.0","-1.0","-1.0")
  let  line = intercalate "," [date,tH,tA,"-1","-1",o1,ox,o2]  ++ " \n"
  return line

-- | Parser for date
parseDate :: Parser String
parseDate = do
  day <- replicateM 2 digit
  char '.'
  month <- replicateM 2 digit
  char '.'
  year <- replicateM 4 digit
  many (noneOf ",")
  char ','
  return $ year++ "." ++ month ++ "." ++day

-- | Parser for teams
parseTeams :: Parser (String, String)
parseTeams = do
  home <- many (noneOf "-")
  char '-'
  away <- many (noneOf ",")
  many (char ',')
  return (strip' home,strip' away)

-- | Parser for goals
parseGoals :: Parser (String, String)
parseGoals = do
  many space
  goalsH <- many digit
  many space >> char ':' >> many space
  goalsA <- many digit
  many space
  return (goalsH, goalsA)

-- | Parser for all odds
parseOdds :: Parser (String, String, String)
parseOdds = do
  many space >> many (char ',')
  odd1 <- odds
  many (char ',')
  oddx <- odds
  many (char ',')
  odd2 <- odds
  return (odd1,oddx,odd2)
  where
    odds = do
      many space
      int <- many digit
      dot <- char '.'
      rest <- many digit
      let odd = int ++ [dot] ++ rest
      return odd
