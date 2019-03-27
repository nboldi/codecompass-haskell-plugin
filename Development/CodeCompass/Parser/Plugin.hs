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
{-# LANGUAGE TupleSections              #-}
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
import HsExpr
import HsDoc
import HsTypes

import Data.Maybe
import Data.Data (toConstr)
import Data.List (nubBy, sortOn, find)
import Data.Function
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.IO.Unlift
import Data.Text hiding (drop, dropWhile, filter, length, map, concatMap, concat, find)
import Control.Monad.Reader
import Control.Monad.Logger
import Conduit
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class
import Control.Lens hiding (Context(..), element)
import GHC.Stack
import TcEvidence

import Development.CodeCompass.Schema

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
          (\e -> do liftIO $ putStrLn $ displayException e
                    liftIO $ putStrLn $ prettyCallStack callStack
                    throwM (e :: SomeException))

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
                      let instSections = catMaybes $ map (fmap (,Instance) . instanceSections) (concatMap group_instds $ hs_tyclds gr)
                          contextSections = concatMap (map (,Context) . getContext) (universeBi gr :: [LHsType GhcRn])
                          sections = instSections ++ contextSections
                      mapM_ (storeName sections mod) sortedNames
                  ) initState
       return (tc, gr)

cleanUp :: Module -> DB ()
cleanUp mod = do
  m <- selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr mod)] []
  case m of
    [modKey] -> do 
      deps <- findDependent modKey
      -- order of deletions is important for foreign key consistency
      deleteWhere [HsTagModule <-. deps]
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

instanceSections :: Located (InstDecl a) -> Maybe RealSrcSpan
instanceSections (L _ (ClsInstD _ inst)) = maybeRealSrcSpan $ findTypeFunLoc (hsib_body $ cid_poly_ty inst)
instanceSections (L _ (DataFamInstD _ inst)) = maybeRealSrcSpan $ getLoc (feqn_tycon $ hsib_body $ dfid_eqn inst)
instanceSections _ = Nothing

findTypeFunLoc :: Located (HsType a) -> SrcSpan
findTypeFunLoc (L _ (HsAppTy _ t _)) = findTypeFunLoc t
findTypeFunLoc (L _ (HsOpTy _ _ (L l _) _)) = l
findTypeFunLoc (L _ (HsParTy _ t)) = findTypeFunLoc t
findTypeFunLoc (L l _) = l

getContext :: Located (HsType a) -> [RealSrcSpan]
getContext (L l (HsQualTy _ ctx _)) = catMaybes $ map (maybeRealSrcSpan . findTypeFunLoc) (unLoc ctx)
getContext _ = []

maybeRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
maybeRealSrcSpan (RealSrcSpan sp) = Just sp
maybeRealSrcSpan _ = Nothing

lookupSection :: [(RealSrcSpan,Tag)] -> SrcSpan -> [Tag]
lookupSection sections (RealSrcSpan sp) = map snd $ filter (\(s,_) -> s `containsSpan` sp) sections

storeName :: HasCallStack => [(RealSrcSpan,Tag)] -> Key HsModule -> Located Name -> ParseM ()
storeName sections mod (L l n) 
  = if isGoodSrcSpan l
    then void $ do 
           Just (myLoc, file) <- insertLoc mod l
           defLoc <- insertLoc mod (nameSrcSpan n)
           let nameStr = showSDocUnsafe (ppr n)
               tags = lookupSection sections l
           name <- insert' always $ HsName mod file nameStr myLoc (fmap fst defLoc)
           mapM_ (\t -> insert' always $ HsTag mod name t) tags
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
       let bindings = universeBi (bagToList (tcg_binds tc))
           expressions = universeBi (bagToList (tcg_binds tc))
           names = universeBi (bagToList (tcg_binds tc))
       liftIO $ putStrLn "### Bindings:"
       liftIO $ mapM (\e -> putStrLn (showSrcSpan (getLoc e) ++ ": " ++ showSDocUnsafe (ppr (e :: LHsBindLR GhcTc GhcTc)) ++ ": " ++ show (toConstr (unLoc e)))) bindings
       liftIO $ putStrLn "### Expressions:"
       liftIO $ mapM (\e -> putStrLn (showSrcSpan (getLoc e) ++ ": " ++ showExpr (unLoc e :: HsExpr GhcTc) ++ ": " ++ show (toConstr (unLoc e)))) expressions
       liftIO $ putStrLn "### Names:"
       liftIO $ mapM (\e -> putStrLn (showSrcSpan (getLoc e) ++ ": " ++ showSDocUnsafe (ppr (e :: Located Name)) ++ ": " ++ show (toConstr (unLoc e)))) names

       return tc
  where showName :: Id -> IO ()
        showName id = putStrLn $ (showSDocUnsafe $ ppr id) ++ ": " ++ (showSDocUnsafe $ ppr (idType id))

        showSrcSpan (RealSrcSpan sp) = showSDocUnsafe (pprUserRealSpan True sp)
        showSrcSpan _ = "-"

        showExpr (HsWrap _ w e) = showSDocUnsafe (ppr e) ++ "(" ++ showWrap w ++ ")"
        showExpr e = showSDocUnsafe (ppr e)

        showWrap WpHole = "WpHole"
        showWrap (WpCompose w1 w2) = "WpCompose (" ++ showWrap w1 ++ ") (" ++ showWrap w2 ++ ")"
        showWrap (WpFun w1 w2 t sd) = "WpFun (" ++ showWrap w1 ++ ") (" ++ showWrap w2 ++ ") (" ++ showSDocUnsafe (ppr t) ++ ") (" ++ showSDocUnsafe sd ++ ")"
        showWrap (WpCast coerce) = "WpCast (" ++ showSDocUnsafe (ppr coerce) ++ ")"
        showWrap (WpEvLam var) = "WpEvLam (" ++ showSDocUnsafe (ppr var) ++ ")"
        showWrap (WpEvApp term) = "WpEvApp (" ++ showSDocUnsafe (ppr term) ++ ")"
        showWrap (WpTyLam tv) = "WpCast (" ++ showSDocUnsafe (ppr tv) ++ ")"
        showWrap (WpTyApp t) = "WpTyApp (" ++ showSDocUnsafe (ppr t) ++ ")"
        showWrap (WpLet binds) = "WpLet (" ++ showSDocUnsafe (ppr binds) ++ ")"

storeImport :: HasCallStack => Key HsModule -> ModuleName -> ParseM ()
storeImport importer modName = void $ do
  imported <- insertWithCache ifNotExist psModules (HsModule $ showSDocUnsafe $ ppr modName)
  void $ insertWithCache always psImports (HsImport importer imported)
