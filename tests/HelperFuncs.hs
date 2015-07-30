module HelperFuncs where

import Control.Exception (bracket, catch, throwIO, )
import Prelude hiding (catch)
import System.Directory (removeFile)
import System.IO.Error

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
