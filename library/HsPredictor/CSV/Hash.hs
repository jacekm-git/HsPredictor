module HsPredictor.CSV.Hash where

-- standard
import           Data.ByteString.Lazy.Char8 (pack)
-- 3rd party
import           Data.Digest.Pure.MD5

{-| Genereates MD5 hash of file contents -}
genHash :: String -- ^ file contents
           -> IO String
genHash fileContent = do
  let md5Digest = md5 $ pack fileContent
  return $ show md5Digest

{-| Checks if old hash == new hash -}
checkHash :: String -> String -> Bool
checkHash hashFile hashDB = hashFile /= hashDB
