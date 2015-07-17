module HashCSV where

-- standard
import Data.ByteString.Lazy.Char8 (pack)
-- 3rd party
import Data.Digest.Pure.MD5

type FileContents = String

genHash :: FileContents -> IO String
genHash fileContent = do
  let md5Digest = md5 $ pack fileContent
  return $ show md5Digest

checkHash :: String -> String -> Bool
checkHash hashFile hashDB = if hashFile == hashDB
                            then False
                            else True




