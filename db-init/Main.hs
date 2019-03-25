
import Development.CodeCompass.Parser.Plugin

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Control.Monad
import System.Directory
import System.Environment
import Data.Text hiding (drop, dropWhile, filter, length, map)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Needs the database to reset"
    db:_ -> do exists <- doesFileExist db 
               when exists (removeFile db)
               runSqlite (pack db) $ runMigration migrateAll
