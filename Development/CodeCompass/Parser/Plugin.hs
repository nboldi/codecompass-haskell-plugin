{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
module Development.CodeCompass.Parser.Plugin where

import DynFlags (getDynFlags)
import Plugins
import HscTypes
import TcRnTypes
import HsExtension
import HsDecls
import HsBinds
import HsExpr
import HsImpExp
import Name
import Id
import Bag
import SrcLoc
import FastString
import Module
import Avail
import Outputable
import HsDoc

import Data.List (nubBy, sortOn)
import Data.Function
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.IO.Unlift
import Data.Text hiding (drop, dropWhile, filter, length, map, concatMap, concat)
import Control.Monad.Reader
import Control.Monad.Logger
import Conduit
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class
import Control.Lens hiding (element)
import GHC.Stack

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
HsFile
    filename String
    module HsModuleId
    deriving Show
    deriving Eq
    deriving Ord
    UniqueFilename filename
HsSourceLoc
    module HsModuleId
    file HsFileId
    startRow Int
    startCol Int
    endRow Int
    endCol Int
    deriving Show
    deriving Eq
    deriving Ord
    UniqueSourceLoc file startRow startCol
HsName
    module HsModuleId
    file HsFileId
    nameStr String
    nameLocation HsSourceLocId
    definedAt HsSourceLocId Maybe
    deriving Show
    deriving Eq
    deriving Ord
    UniqueName nameLocation
HsModule
    moduleName String
    deriving Show
    deriving Eq
    deriving Ord
    UniqueModule moduleName
HsImport
    importer HsModuleId
    imported HsModuleId
    deriving Show
    deriving Eq
    deriving Ord
    UniqueImport importer imported
|]

type Cache a = Map.Map a (Key a)

data ParseState = PS { _psFile      :: Cache HsFile
                     , _psSourceLoc :: Cache HsSourceLoc
                     , _psModules   :: Cache HsModule
                     , _psImports   :: Cache HsImport
                     }

makeLenses ''ParseState

plugin :: Plugin
plugin = defaultPlugin {
    -- parsedResultAction = parsed
  typeCheckResultAction = typecheckPlugin
  -- , spliceRunAction = spliceRun
  -- , interfaceLoadAction = interfaceLoad
  , renamedResultAction = renamedAction
  }

insertWithCache :: (Ord a, PersistEntity a, PersistRecordBackend a SqlBackend, HasCallStack, Show a)
                => (a -> DB (Key a)) -> (Lens' ParseState (Cache a)) -> a -> ParseM (Key a)
insertWithCache insertMethod ref elem = do
  mkey <- gets (Map.lookup elem . view ref)
  case mkey of Just key -> return key
               Nothing -> do key <- insert' insertMethod elem
                             modify (over ref (Map.insert elem key))
                             return key

ifNotExist :: PersistRecordBackend a SqlBackend => a -> DB (Key a)
ifNotExist elem = either entityKey id <$> insertBy elem

always :: PersistRecordBackend a SqlBackend => a -> DB (Key a)
always = insert

insert' :: (HasCallStack, PersistRecordBackend a SqlBackend, Show a) => (a -> DB (Key a)) -> a -> ParseM (Key a)
-- TODO: the check before the insert is not always needed, might boost performance to omit
insert' insertMethod elem
  = catch ({- do liftIO (putStrLn ("inserting" ++ (show elem))); -} lift (insertMethod elem))
          (\e -> throwM (e :: SomeException))

type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

type ParseM = StateT ParseState DB

initState :: ParseState
initState = PS Map.empty Map.empty Map.empty Map.empty

renamedAction :: HasCallStack => [CommandLineOption]
                    -> TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction [] _ _
  = error "CodeCompass plugin needs a connection string to a database to store the data."
renamedAction (connString:_) tc gr
  = liftIO $ connectDB connString $ do
       let names = universeBi gr
           sortedNames = nubBy ((==) `on` getLoc) $ sortOn getLoc names
       cleanUp (tcg_mod tc)
       evalStateT (do mod <- insertModule (tcg_mod tc)
                      mapM_ (storeName mod) sortedNames) initState
       return (tc, gr)

cleanUp :: Module -> DB ()
cleanUp mod = do
  m <- selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr mod)] []
  case m of
    [modKey] -> do 
      deps <- findDependent modKey
      -- order of deletions is important for foreign key consistency
      deleteWhere [HsNameModule <-. deps]
      deleteWhere [HsSourceLocModule <-. deps]
      deleteWhere [HsFileModule <-. deps]
      deleteWhere [HsImportImporter <-. deps]
      mapM_ delete deps
    _ -> return ()


findDependent :: Key HsModule -> DB [Key HsModule]
findDependent m = do
  newKeys <- selectList [HsImportImported ==. m] []
  furtherKeys <- mapM findDependent (map (hsImportImporter . entityVal) newKeys)
  return (m : concat furtherKeys) 

insertModule :: Module -> ParseM (Key HsModule)
insertModule m = insert' always $ HsModule (showSDocUnsafe (ppr m))     

connectDB :: HasCallStack => String -> DB a -> IO a
connectDB connString = runSqlite (pack (drop 1 $ dropWhile (/='=') connString))

storeName :: HasCallStack => Key HsModule -> Located Name -> ParseM ()
storeName mod (L l n) 
  = if isGoodSrcSpan l
    then void $ do 
           Just (myLoc, file) <- insertLoc mod l
           defLoc <- insertLoc mod (nameSrcSpan n)
           let nameStr = showSDocUnsafe (ppr n)
           insert' always $ HsName mod file nameStr myLoc (fmap fst defLoc)
    else return ()

insertLoc :: HasCallStack => Key HsModule -> SrcSpan -> ParseM (Maybe (Key HsSourceLoc, Key HsFile))
insertLoc mod (RealSrcSpan rsp) = do
  file <- insertFile mod (unpackFS (srcSpanFile rsp))
  let sloc = HsSourceLoc mod file (srcSpanStartLine rsp) (srcSpanStartCol rsp) 
                         (srcSpanEndLine rsp) (srcSpanEndCol rsp)
  sl <- insertWithCache ifNotExist psSourceLoc sloc -- sometimes the precise source spans do not match
  return $ Just (sl,file)
insertLoc _ _ = return Nothing

insertFile :: Key HsModule -> String -> ParseM (Key HsFile)
insertFile mod str = insertWithCache ifNotExist psFile (HsFile str mod)

typecheckPlugin :: HasCallStack => [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin (connString:_) ms tc
  = liftIO $ connectDB connString $ evalStateT (storeTC ms tc) initState

storeTC :: HasCallStack => ModSummary -> TcGblEnv -> ParseM TcGblEnv
storeTC ms tc 
  = do [modKey] <- lift $ selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr (ms_mod ms))] []
       mapM_ (storeImport modKey) (map (unLoc . snd) (ms_textual_imps ms))
       --liftIO $ putStrLn $ "typeCheckPlugin (rn): \n" ++ (showSDoc dflags $ ppr $ tcg_rn_decls tc)
       --liftIO $ putStrLn $ "typeCheckPlugin (tc): \n" ++ (showSDocUnsafe $ ppr $ tcg_binds tc)
       --let names = universeBi (bagToList (tcg_binds tc))
       --liftIO $ mapM showName names
       return tc
  where showName :: Id -> IO ()
        showName id = putStrLn $ (showSDocUnsafe $ ppr id) ++ ": " ++ (showSDocUnsafe $ ppr (idType id))

storeImport :: HasCallStack => Key HsModule -> ModuleName -> ParseM ()
storeImport importer modName = void $ do
  imported <- insertWithCache ifNotExist psModules (HsModule $ showSDocUnsafe $ ppr modName)
  void $ insertWithCache always psImports (HsImport importer imported)

