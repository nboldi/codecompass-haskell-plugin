{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Development.CodeCompass.Parser.Plugin

import LanguageService
import LanguageService_Iface
import Language_Types
import Common_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Server

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Sqlite (Connection, open, close)

import qualified Data.Vector as Vector
import qualified Data.Text.Lazy as Text
import qualified Data.Text as StrictText
import Data.Int
import Data.String
import Data.Maybe
import Text.Printf
import Control.Monad.IO.Class
import Control.Exception (throw, bracket, catch, SomeException)
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Map ((!))
import Data.Monoid
import System.Environment

data HaskellLanguageService backend
  = HaskellLanguageService { hslConn :: Database.Sqlite.Connection, hslBackend :: backend }

data Reference = Name
  deriving (Eq, Show, Enum)

newSqliteHaskellLanguageService :: String -> IO (HaskellLanguageService SqlBackend)
newSqliteHaskellLanguageService connstr = do
  conn <- open (StrictText.pack connstr)
  backend <- wrapConnection conn (\_ _ _ _ -> return ())
  node <- flip runSqlPersistM backend $ selectList [] [LimitTo 1]
  print (node :: [Entity HsFile])
  return $ HaskellLanguageService conn (backend :: SqlBackend)

closeSqliteHaskellLanguageService :: HaskellLanguageService SqlBackend -> IO ()
closeSqliteHaskellLanguageService = close . hslConn

runWithDB :: HaskellLanguageService SqlBackend -> DB a -> IO a
runWithDB hsl db = catch (flip runSqlPersistM (hslBackend hsl) db)
                         (\e -> do putStrLn (show (e :: SomeException))
                                   throw e)

hsSourceLocToRange :: HsFile -> HsSourceLoc -> FileRange
hsSourceLocToRange (HsFile fl) (HsSourceLoc _ sr sc er ec)
  = FileRange (Text.pack fl) (Range (Position (fromIntegral sr) (fromIntegral sc))
                                    (Position (fromIntegral er) (fromIntegral ec)))

hsSourceLocToNodeInfo :: HsFile -> Entity HsSourceLoc -> AstNodeInfo
hsSourceLocToNodeInfo fl sl
  = AstNodeInfo (Text.pack $ show $ entityKey sl) 0 (Text.pack "name") (Text.pack "name") "" (hsSourceLocToRange fl (entityVal sl)) Vector.empty



instance LanguageService_Iface (HaskellLanguageService SqlBackend) where
  getFileTypes _ = return $ Vector.fromList [Text.pack "HS"]

  getAstNodeInfo _ _ = return default_AstNodeInfo
  
  getAstNodeInfoByPosition s (FilePosition f (Position l c)) =
    runWithDB s $ do 
      file <- selectList [HsFileFilename ==. Text.unpack f] [LimitTo 1]
      case file of
        fl:_ -> do
          node <- selectList [HsSourceLocFile ==. entityKey fl, HsSourceLocStartRow ==. fromIntegral l, HsSourceLocStartCol <=. fromIntegral c, HsSourceLocEndCol >=. fromIntegral c] [Desc HsSourceLocStartCol, LimitTo 1]
          case node of 
            n:_ -> return $ hsSourceLocToNodeInfo (entityVal fl) n
            [] -> throw $ InvalidInput $ Text.pack $ "AST node not found: " ++ Text.unpack f ++ " " ++ show l ++ ":" ++ show c
        
  getSourceText _ _ = return Text.empty
  getDocumentation _ _ = return Text.empty
  getProperties _ _ = return HM.empty
  getDiagramTypes _ _ = return HM.empty
  getDiagram _ _ _ = return Text.empty
  getDiagramLegend _ _ = return Text.empty
  getFileDiagramTypes _ _ = return HM.empty
  getFileDiagram _ _ _ = return Text.empty
  getFileDiagramLegend _ _ = return Text.empty
  
  getReferenceTypes s k = return $ HM.fromList [ (Text.pack (show Name), fromIntegral (fromEnum Name)) ]
  
  getReferenceCount s k r = do refs <- getReferences s k r Vector.empty
                               return (fromIntegral $ Vector.length refs)

  getReferences s k (toEnum . fromIntegral -> Name) _ =
    runWithDB s $ do
      let loc = read $ Text.unpack k
      name <- selectList [HsNameNameLocation ==. loc] [LimitTo 1]
      target <- case name of [n] | Just dl <- hsNameDefinedAt (entityVal n) -> getEntity dl
                             _ -> throw $ InvalidInput $ Text.pack $ "Name not found."
      case target of Just tr -> do Just fl <- get (hsSourceLocFile (entityVal tr))
                                   return $ Vector.fromList [ hsSourceLocToNodeInfo fl tr ]
                     Nothing -> return $ Vector.empty

  getReferencesInFile _ _ _ _ _ = return Vector.empty
  getReferencesPage _ _ _ _ _ = return Vector.empty
  getFileReferenceTypes _ _ = return HM.empty
  getFileReferences _ _ _ = return Vector.empty
  getFileReferenceCount _ _ _ = return $ fromIntegral 0
  getSyntaxHighlight _ _ = return Vector.empty

main = do
  args <- getArgs
  case args of
    [connStr] -> bracket 
        (newSqliteHaskellLanguageService connStr)
        (\handler -> do
            print "Starting Haskell Language Service server..."
            runBasicServer handler LanguageService.process 9090
        )
      closeSqliteHaskellLanguageService
    _ -> error "HaskellLanguageService needs 1 argument, the database connection string."
