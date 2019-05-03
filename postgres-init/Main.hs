
import Development.CodeCompass.Parser.Utils

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Development.CodeCompass.Schema

import Control.Monad
import System.Directory
import System.Environment
import Data.Text hiding (drop, dropWhile, filter, length, map, intercalate)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.List (intercalate)
import Control.Monad.Logger
import Control.Monad.Trans.Resource

main :: IO ()
main = do
  args <- getArgs
  let conn = intercalate " " args
  case args of
    [] -> putStrLn "Needs the database to reset"
    _ -> runResourceT $ runNoLoggingT $ withPostgresqlPool (BS.pack conn) 1 (runSqlPool cleanAndMigrate)
  where cleanAndMigrate = do
          runMigration migrateAll
          removeAll
