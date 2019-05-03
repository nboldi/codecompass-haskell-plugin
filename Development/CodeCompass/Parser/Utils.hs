{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}

module Development.CodeCompass.Parser.Utils where

import Module
import Outputable

import Conduit
import Control.Lens hiding (Context(..), element)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS (pack)
import Data.List (intercalate)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Time.Clock
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sqlite
import GHC.Stack
import Numeric
import System.Directory
import System.IO
import System.TimeIt

import Development.CodeCompass.Schema as Schema

-- | Store the values that are inserted to the database during this export stage.
data ParseState = PS { _psFile      :: Cache HsFile
                     , _psSourceLoc :: Cache HsSourceLoc
                     , _psModules   :: Cache HsModule
                     , _psImports   :: Cache HsImport
                     }

-- | Store one type of values that are inserted to the database during this export stage.
type Cache a = Map.Map a (Key a)

makeLenses ''ParseState

-- | Connect to an sqlite or postgresql database based on the plugin arguments
connectDB :: HasCallStack => [String] -> DB a -> IO a
connectDB (connString:_) action
  | "sqlite" `isPrefixOf` connString = runSqlite (pack (drop 1 $ dropWhile (/='=') connString)) action
connectDB arguments action
  = runResourceT $ runNoLoggingT $ withPostgresqlPool (BS.pack connStr) 1 (runSqlPool action)
  where connStr = intercalate " " arguments

-- | If the element has been already inserted, retrieve its key, otherwise insert it
insertWithCache :: (Ord a, PersistEntity a, PersistRecordBackend a SqlBackend, HasCallStack, Show a)
                => (Lens' ParseState (Cache a)) -> a -> ParseM (Key a)
insertWithCache ref elem = do
  mkey <- gets (Map.lookup elem . view ref)
  case mkey of Just key -> return key
               Nothing -> do key <- lift (either entityKey id <$> insertBy elem)
                             modify (over ref (Map.insert elem key))
                             return key

-- | Insert if element is not already in the database
ifNotExist :: PersistRecordBackend a SqlBackend => a -> DB (Key a)
ifNotExist elem = either entityKey id <$> insertBy elem

-- | Insert every time
always :: PersistRecordBackend a SqlBackend => a -> DB (Key a)
always = insert

-- | Insert with the behavior specified when the element is already in the database
insert' :: (HasCallStack, PersistRecordBackend a SqlBackend, Show a)
        => (a -> DB (Key a)) -> a -> ParseM (Key a)
-- TODO: the check before the insert is not always needed, might boost performance to omit
insert' insertMethod elem = lift (insertMethod elem)

-- | Monad accessing the database
type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

-- | Monad accessing the database with insert caches
type ParseM = StateT ParseState DB

-- | Initial state of the caches
initState :: ParseState
initState = PS Map.empty Map.empty Map.empty Map.empty

-- | Store how long does the operation take in the database
wrapTiming :: (HasCallStack, MonadIO m) => Module -> String -> (PluginLogEvent -> m ()) -> m a -> m a
wrapTiming mod msg insert action = do
  (time,res) <- timeItT action
  insert (PluginLogEvent LogInfo (showSDocUnsafe (ppr mod)) (message time))
  return res
  where message time = showFFloat (Just 4) time "" ++ " : " ++ msg

-- | Insert in a lifted monad (used for 'wrapTiming')
insIn :: PluginLogEvent -> ParseM ()
insIn = void . lift . insert

-- | Handle all errors and try to log them to the database
wrapErrorHandling :: HasCallStack => Module -> DB () -> DB a -> DB a
wrapErrorHandling mod action finale
    = catch action (\e -> ignoreError e (void $ insert (PluginLogEvent LogError modName (errorMsg e))))
        >> finale
  where modName = showSDocUnsafe (ppr mod)
        errorMsg e = displayException (e :: SomeException) ++ "\n" ++ prettyCallStack callStack
        ignoreError orig act = catch act (\e -> do liftIO $ hPutStrLn stderr "Error occurred:"
                                                   liftIO $ hPutStrLn stderr $ displayException (e :: SomeException)
                                                   liftIO $ hPutStrLn stderr "While logging error:"
                                                   liftIO $ hPutStrLn stderr (errorMsg orig))

-- | Only perform the action if the module had not been exported since the source file was changed
lazyStore :: Module -> Either FilePath UTCTime -> DB () -> DB ()
lazyStore mod fpOrTime action = do
  findMod <- selectList [PluginExportEventExportName ==. showSDocUnsafe (ppr mod)] []
  case findMod of
    (entityVal -> PluginExportEvent _ time) : _ -> do
      modifyTime <- either (liftIO . getModificationTime) return fpOrTime
      when (modifyTime > time) action
    [] -> action

-- | Remove all data related to the module or any modules that depend on it
cleanUp :: HasCallStack => Module -> DB ()
cleanUp mod = do
  m <- selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr mod)] []
  case m of
    [modKey] -> do 
      deps <- findDependent modKey
       -- remove circular dependency
      mapM (\m -> update m [ HsModuleModNameLoc =. Nothing ]) deps
      deleteWhere [HsCommentModule <-. deps]
      deleteWhere [HsInstanceInvokationModule <-. deps]
      deleteWhere [HsTagModule <-. deps]
      deleteWhere [HsNameModule <-. deps]
      deleteWhere [HsSourceLocModule <-. deps]
      deleteWhere [HsFileModule <-. deps]
      deleteWhere [HsImportImporter <-. deps]
      mapM_ delete deps
    _ -> return ()

-- Find all modules that are depending on a certain module (recursively)
findDependent :: HasCallStack => Key HsModule -> DB [Key HsModule]
findDependent m = do
  newKeys <- selectList [HsImportImported ==. m] []
  furtherKeys <- mapM findDependent (map (hsImportImporter . entityVal) newKeys)
  return (m : concat furtherKeys) 

-- | Clear the database
removeAll :: HasCallStack => DB ()
removeAll = do 
  -- remove circular dependency
  updateWhere [] [ HsModuleModNameLoc =. Nothing ]
  deleteWhere ([] :: [Filter HsComment])
  deleteWhere ([] :: [Filter HsInstanceInvokation])
  deleteWhere ([] :: [Filter HsTag])
  deleteWhere ([] :: [Filter HsName])
  deleteWhere ([] :: [Filter HsSourceLoc])
  deleteWhere ([] :: [Filter HsFile])
  deleteWhere ([] :: [Filter HsImport])
  deleteWhere ([] :: [Filter HsModule])
  deleteWhere ([] :: [Filter PluginLogEvent])
  deleteWhere ([] :: [Filter PluginExportEvent])