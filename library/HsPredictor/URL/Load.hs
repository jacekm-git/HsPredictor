module HsPredictor.URL.Load where
--standard
import           Control.Exception             (catch, throwIO)
import           Prelude                       hiding (catch)
--3rd party
import           Data.ByteString.Lazy.Char8    (pack, unpack)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Network.HTTP.Conduit

-- | Return body of html page
getBody :: String -- ^ url
           -> IO String
getBody url = do
  x <- simpleHttp url `catch` handleError
  return $ unpack x
  where
    handleError :: HttpException -> IO ByteString
    handleError e = return $ pack "wrong url or no connection"
