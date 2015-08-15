module LoadURL where
--standard
import Control.Exception (catch, throwIO)
import Prelude hiding (catch)
--3rd party
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8(unpack, pack)
import Data.ByteString.Lazy.Internal(ByteString)

getBody :: String -> IO String
getBody url = do
  x <- simpleHttp url `catch` handleError
  return $ unpack x
  where
    handleError :: HttpException -> IO ByteString
    handleError e = return $ pack "wrong url or no connection"
