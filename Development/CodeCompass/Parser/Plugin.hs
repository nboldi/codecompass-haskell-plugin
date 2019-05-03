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
{-# LANGUAGE ViewPatterns               #-}
module Development.CodeCompass.Parser.Plugin where

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
import IdInfo
import Var (varName, varType)
import Bag
import SrcLoc
import FastString
import Module
import Avail
import Outputable
import DynFlags
import StringBuffer
import HsExpr
import HsDoc
import Parser
import ApiAnnotation
import Lexer
import HsTypes
import InstEnv
import qualified CoreSyn as Core (Expr)
import CoreSyn as Core
import HsSyn hiding (HsModule)
import RdrName
import HscTypes
import UniqFM
import Unique
import NameEnv
import Type

import Numeric
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe
import Data.IORef
import Data.Data (toConstr)
import Data.List (nubBy, sortOn, find, isPrefixOf, minimumBy, maximumBy)
import Data.Function
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.IO.Unlift
import Data.Text (pack)
import qualified Data.ByteString.Char8 as BS (pack)
import Control.Monad.Reader
import Control.Monad.Logger
import Conduit
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class
import Control.Lens hiding (Context(..), element)
import GHC.Stack
import TcEvidence
import System.IO
import System.TimeIt
import Data.Time.Clock
import System.Directory

import Development.CodeCompass.Schema as Schema
import Development.CodeCompass.Parser.Names

type Cache a = Map.Map a (Key a)

data ParseState = PS { _psFile      :: Cache HsFile
                     , _psSourceLoc :: Cache HsSourceLoc
                     , _psModules   :: Cache HsModule
                     , _psImports   :: Cache HsImport
                     }

makeLenses ''ParseState

plugin :: Plugin
plugin = defaultPlugin {
  parsedResultAction = parsedAction
  , typeCheckResultAction = typecheckPlugin
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
          (\e -> do liftIO $ putStrLn $ displayException e  -- this could be removed
                    liftIO $ putStrLn $ prettyCallStack callStack -- this could be removed
                    throwM (e :: SomeException)) -- TODO: add info about what is inserted

type DB = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

type ParseM = StateT ParseState DB

initState :: ParseState
initState = PS Map.empty Map.empty Map.empty Map.empty

wrapTiming :: (HasCallStack, MonadIO m) => Module -> String -> (PluginLogEvent -> m ()) -> m a -> m a
wrapTiming mod msg insert action = do
  (time,res) <- timeItT action
  insert (PluginLogEvent LogInfo (showSDocUnsafe (ppr mod)) (message time))
  return res
  where message time = showFFloat (Just 4) time "" ++ " : " ++ msg

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

lazyStore :: Module -> Either FilePath UTCTime -> DB () -> DB ()
lazyStore mod fpOrTime action = do
  findMod <- selectList [PluginExportEventExportName ==. showSDocUnsafe (ppr mod)] []
  case findMod of
    (entityVal -> PluginExportEvent _ time) : _ -> do
      modifyTime <- either (liftIO . getModificationTime) return fpOrTime
      when (modifyTime > time) action
    [] -> action

parsedAction :: HasCallStack => [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction [] _ _
  = error "CodeCompass plugin needs a connection string to a database to store the data."
parsedAction arguments ms parse
  = liftIO $ connectDB arguments $ wrapErrorHandling (ms_mod ms) (lazyStore (ms_mod ms) (Right (ms_hs_date ms)) $ do 
      wrapTiming (ms_mod ms) "cleanUp" (void . insert) $ cleanUp (ms_mod ms)
      flip evalStateT initState $ do
        mod <- wrapTiming (ms_mod ms) "insertModule" insIn $ insertModule (ms_mod ms) (hsmodName (unLoc $ hpm_module parse))
        comments <- wrapTiming (ms_mod ms) "parseForComments" insIn $ liftIO $ parseForComments ms
        let withAttach = commentAttach comments
            moduleItself = (\n -> (getLoc n, getLoc n)) <$> hsmodName (unLoc $ hpm_module parse)
            attachableNames = map (\(n,sp) -> (getLoc n, sp)) $ hsGetNames noSrcSpan 
                           $ hsmodDecls $ unLoc $ hpm_module parse
            attachable = catMaybes [moduleItself] ++ attachableNames
        wrapTiming (ms_mod ms) "attach" insIn $ mapM_ (attach mod attachable) withAttach
        wrapTiming (ms_mod ms) "storeModuleName" insIn $ mapM_ (storeModuleName mod) (universeBi (hpm_module parse)))
      (return parse)

insIn = void . lift . insert

storeModuleName :: HasCallStack => Key HsModule -> Located ModuleName -> ParseM ()
storeModuleName mod (L l modName) = do 
  store <- insertLoc mod l
  findMod <- lift $ selectList [HsModuleModuleName ==. moduleNameString modName] []
  case (store, findMod) of
    (Just (loc, _), (entityVal -> HsModule _ modNameLoc):_) 
      -> void $ insert' ifNotExist $ HsName mod (moduleNameString modName) loc modNameLoc Nothing Nothing
    _ -> return ()

attach :: HasCallStack => Key HsModule -> [(SrcSpan, SrcSpan)] -> Located (AnnotationComment, AttachRule) -> ParseM ()
attach mod elements (L commLoc (comm, attach)) 
  = case filter (attachCondition . snd) elements of
      [] -> return ()
      elems -> do locs <- mapM (insertLoc mod . fst) (attachSelect elems)
                  void $ mapM (insert' always) (map (\(l,_) -> HsComment mod l (getCommentString comm)) (catMaybes locs))
  where attachCondition = case attach of AttachNext -> (srcSpanEnd commLoc <) . srcSpanStart
                                         AttachPrevious -> (srcSpanStart commLoc >) . srcSpanEnd
        attachSelect ls = case attach of AttachNext -> filter ((== minimumBy (compare `on` srcSpanStart) (map snd ls)) . snd) ls
                                         AttachPrevious -> filter ((== maximumBy (compare `on` srcSpanEnd) (map snd ls)) . snd) ls

commentAttach :: HasCallStack => [Located AnnotationComment] -> [Located (AnnotationComment, AttachRule)]
commentAttach = catMaybes . map (\(L l e) -> fmap (L l) $ categorizeComment e) . foldr joinComments []
  where joinComments :: (Located AnnotationComment) -> [Located AnnotationComment] -> [Located AnnotationComment]
        joinComments (L sp1@(RealSrcSpan (srcSpanStartLine -> r1)) (AnnLineComment str1))
                     (L sp2@(RealSrcSpan (srcSpanStartLine -> r2)) (AnnLineComment str2) : ls) 
          | abs (r1 - r2) <= 1
          , not $ "|" `isPrefixOf` dropWhile isSpace (drop 2 str2)
          , not $ "^" `isPrefixOf` dropWhile isSpace (drop 2 str2)
          = L (combineSrcSpans sp2 sp1) (AnnLineComment (str2 ++ "\n" ++ str1)) : ls
        joinComments comm [] = [comm]
        joinComments comm ls = comm : ls

getCommentString :: AnnotationComment -> String
getCommentString (AnnLineComment str) = str
getCommentString (AnnBlockComment str) = str

data AttachRule = AttachNext | AttachPrevious
  deriving Show

instance Outputable AttachRule where
  ppr = text . show

categorizeComment :: AnnotationComment -> Maybe (AnnotationComment, AttachRule)
categorizeComment comm@(AnnLineComment str) 
  | "|" `isPrefixOf` dropWhile isSpace (drop 2 str) =  Just (comm, AttachNext)
categorizeComment comm@(AnnLineComment str) 
  | "^" `isPrefixOf` dropWhile isSpace (drop 2 str) =  Just (comm, AttachPrevious)
categorizeComment comm@(AnnBlockComment str) 
  | "|" `isPrefixOf` dropWhile isSpace (drop 2 str) =  Just (comm, AttachNext)
categorizeComment comm@(AnnBlockComment str) 
  | "^" `isPrefixOf` dropWhile isSpace (drop 2 str) =  Just (comm, AttachPrevious)
categorizeComment _ = Nothing

parseForComments :: ModSummary -> IO [Located AnnotationComment]
parseForComments ms = do
  let dflags = ms_hspp_opts ms
      dflags' = gopt_set dflags Opt_KeepRawTokenStream
      file = msHsFilePath ms
      location = mkRealSrcLoc (mkFastString file) 1 1
  buffer <- hGetStringBuffer file
  let parseState = mkPState dflags' buffer location
  case unP parseModule parseState of
    POk st _ -> return (comment_q st ++ concatMap snd (annotations_comments st))
    PFailed _ _ _ -> return []

renamedAction :: HasCallStack => [CommandLineOption]
                    -> TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction arguments tc gr
  = liftIO $ connectDB arguments $ wrapErrorHandling (tcg_mod tc) (lazyStore (tcg_mod tc) (Left sourceName) $ void $ do
       let names = universeBi gr
           lieNames = universeBi (tcg_rn_exports tc) ++ universeBi (tcg_rn_imports tc) :: [Located Name]
           sortedNames = nubBy ((==) `on` getLoc) $ filter (isGoodSrcSpan . getLoc) $ sortOn getLoc (names ++ lieNames)
       evalStateT (do [mod] <- lift $ selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr (tcg_mod tc))] []
                      let instSections = catMaybes $ map (fmap (,Instance) . instanceSections) (concatMap group_instds $ hs_tyclds gr)
                          contextSections = concatMap (map (,Context) . getContext) (universeBi gr :: [LHsType GhcRn])
                          sections = instSections ++ contextSections
                      wrapTiming (tcg_mod tc) "storeName" insIn $ mapM_ (storeName sections mod) sortedNames
                  ) initState)
       (return (tc, gr))
  where sourceName = unpackFS $ srcSpanFile $ tcg_top_loc tc

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

findDependent :: HasCallStack => Key HsModule -> DB [Key HsModule]
findDependent m = do
  newKeys <- selectList [HsImportImported ==. m] []
  furtherKeys <- mapM findDependent (map (hsImportImporter . entityVal) newKeys)
  return (m : concat furtherKeys) 

insertModule :: HasCallStack => Module -> Maybe (Located ModuleName) -> ParseM (Key HsModule)
insertModule m Nothing = insert' always $ HsModule (showSDocUnsafe (ppr m)) Nothing
insertModule m (Just name) = do 
  modKey <- insert' always $ HsModule (showSDocUnsafe (ppr m)) Nothing
  loc <- insertLoc modKey (getLoc name)
  case loc of Just (l,_) -> lift $ update modKey [HsModuleModNameLoc =. Just l]
              Nothing -> return ()
  return modKey

connectDB :: HasCallStack => [String] -> DB a -> IO a
connectDB (connString:_) action
  | "sqlite" `isPrefixOf` connString = runSqlite (pack (drop 1 $ dropWhile (/='=') connString)) action
connectDB arguments action
  = runResourceT $ runNoLoggingT $ withPostgresqlPool (BS.pack connStr) 1 (runSqlPool action)
  where connStr = intercalate " " arguments

instanceSections :: HasCallStack => Located (InstDecl a) -> Maybe RealSrcSpan
instanceSections (L _ (ClsInstD _ inst)) = maybeRealSrcSpan $ findTypeFunLoc (hsib_body $ cid_poly_ty inst)
instanceSections (L _ (DataFamInstD _ inst)) = maybeRealSrcSpan $ getLoc (feqn_tycon $ hsib_body $ dfid_eqn inst)
instanceSections _ = Nothing

findTypeFunLoc :: HasCallStack => Located (HsType a) -> SrcSpan
findTypeFunLoc (L _ (HsAppTy _ t _)) = findTypeFunLoc t
findTypeFunLoc (L _ (HsOpTy _ _ (L l _) _)) = l
findTypeFunLoc (L _ (HsParTy _ t)) = findTypeFunLoc t
findTypeFunLoc (L l _) = l

getContext :: HasCallStack => Located (HsType a) -> [RealSrcSpan]
getContext (L l (HsQualTy _ ctx _)) = catMaybes $ map (maybeRealSrcSpan . findTypeFunLoc) (unLoc ctx)
getContext _ = []

maybeRealSrcSpan :: HasCallStack => SrcSpan -> Maybe RealSrcSpan
maybeRealSrcSpan (RealSrcSpan sp) = Just sp
maybeRealSrcSpan _ = Nothing

lookupSection :: [(RealSrcSpan,Tag)] -> SrcSpan -> [Tag]
lookupSection sections (RealSrcSpan sp) = map snd $ filter (\(s,_) -> s `containsSpan` sp) sections

storeName :: HasCallStack => [(RealSrcSpan,Tag)] -> Key HsModule -> Located Name -> ParseM ()
storeName sections mod (L l n) 
  = if isGoodSrcSpan l
    then void $ do 
           Just (myLoc, _) <- insertLoc mod l
           defLoc <- insertLoc mod (nameSrcSpan n)
           let nameStr = showSDocUnsafe (ppr n)
               tags = lookupSection sections l
                           -- why does it need ifNotExist?
           name <- insert' ifNotExist $ HsName mod nameStr myLoc (fmap fst defLoc) Nothing Nothing
           mapM_ (\t -> insert' ifNotExist $ HsTag mod name t) tags
    else return ()

insertLoc :: HasCallStack => Key HsModule -> SrcSpan -> ParseM (Maybe (Key HsSourceLoc, Key HsFile))
insertLoc mod (RealSrcSpan rsp) = do
  file <- insertFile mod (unpackFS (srcSpanFile rsp))
  sl <- insertWithCache ifNotExist psSourceLoc (srcSpanToLoc mod file rsp)
  return $ Just (sl,file)
insertLoc _ _ = return Nothing

srcSpanToLoc :: HasCallStack => Key HsModule -> Key HsFile -> RealSrcSpan -> HsSourceLoc
srcSpanToLoc mod file rsp 
  = HsSourceLoc mod file (srcSpanStartLine rsp) (srcSpanStartCol rsp) 
                         (srcSpanEndLine rsp) (srcSpanEndCol rsp)

insertFile :: HasCallStack => Key HsModule -> String -> ParseM (Key HsFile)
insertFile mod str = insertWithCache ifNotExist psFile (HsFile str mod)

typecheckPlugin :: HasCallStack => [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typecheckPlugin arguments ms tc
  = liftIO $ connectDB arguments
           $ wrapErrorHandling (tcg_mod tc)
               (lazyStore (ms_mod ms) (Right (ms_hs_date ms))
                 $ wrapTiming (ms_mod ms) "storeTypes" (void . insert)
                 $ do evalStateT (storeTC ms tc) initState
                      time <- liftIO getCurrentTime
                      void $ upsert (PluginExportEvent (showSDocUnsafe $ ppr $ ms_mod ms) time)
                                    [ PluginExportEventExportTime =. time ]
                   ) (return tc)

storeTC :: HasCallStack => ModSummary -> TcGblEnv -> ParseM ()
storeTC ms tc 
  = do [modKey] <- lift $ selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr (ms_mod ms))] []
       mapM_ (storeImport modKey) (map (unLoc . snd) (ms_textual_imps ms))

       -- TODO: why doesn't uniplate work on bags and how can this be fixed
       let expressions = filter (isGoodSrcSpan . getLoc) $ universeBi $ bagToList (tcg_binds tc)
           names = universeBi (bagToList (tcg_binds tc))
           implicitTypeArgs = catMaybes (map exprTypeApps expressions)
       mapM (storeInstanceInvokation modKey) $ catMaybes
         $ map (exprInstanceBind (tcg_ev_binds tc)) expressions
       mapM_ (updateNameType modKey implicitTypeArgs) (names ++ catMaybes (map exprName expressions))
  where 
        exprInstanceBind :: HasCallStack => Bag EvBind -> LHsExpr GhcTc -> Maybe (SrcSpan, SrcSpan)
        exprInstanceBind evBinds (L l (HsWrap _ w _))
          | Just (Var id) <- wrapEvApp w
          , EvBind { eb_rhs = EvExpr (Var dictId) } : _ <- bagToList (filterBag (\ev -> eb_lhs ev == id) evBinds)
          = Just (l, nameSrcSpan (Var.varName dictId))
        exprInstanceBind _ _ = Nothing

        exprTypeApps :: HasCallStack => LHsExpr GhcTc -> Maybe (SrcSpan, [Type])
        exprTypeApps (L l (HsWrap _ w _))
          = case wrapTyApp w of [] -> Nothing
                                tys -> Just (l, tys)
        exprTypeApps _ = Nothing

        wrapEvApp :: HasCallStack => HsWrapper -> Maybe EvExpr
        wrapEvApp (WpCompose w1 w2) = wrapEvApp w1 <|> wrapEvApp w2
        wrapEvApp (WpEvApp (EvExpr expr)) = Just expr
        wrapEvApp _ = Nothing

        wrapTyApp :: HasCallStack => HsWrapper -> [Type]
        wrapTyApp (WpCompose w1 w2) = wrapTyApp w1 ++ wrapTyApp w2
        wrapTyApp (WpTyApp t) = [t]
        wrapTyApp _ = []

        updateNameType :: HasCallStack => Key HsModule -> [(SrcSpan, [Type])] -> Located Id -> ParseM ()
        updateNameType mod typeArgs (L sp var)
          = do loc <- insertLoc mod sp
               let nameType = varType var
                   actualArgs = lookup sp typeArgs
                   numArgs = length $ fst $ splitForAllTys (varType var)
                   concretiseType :: HasCallStack => Type -> Type
                   concretiseType t = maybe t (\args -> if length args <= numArgs then piResultTys t args else t) actualArgs
               case loc of Just (l,_) -> lift $ updateWhere
                                           [ HsNameNameLocation ==. l ]
                                           [ HsNameType =. Just (showSDocUnsafe (ppr nameType))
                                           , HsNameConcreteType =. Just (showSDocUnsafe (ppr (concretiseType nameType)))
                                           ]
                           _ -> return ()

        exprName :: HasCallStack => LHsExpr GhcTc -> Maybe (Located Id)
        exprName (L l (HsWrap _ _ w)) = exprName (L l w)
        exprName (L l (HsVar _ id)) = Just $ L l $ unLoc id
        exprName _ = Nothing

storeInstanceInvokation :: HasCallStack => Key HsModule -> (SrcSpan, SrcSpan) -> ParseM ()
storeInstanceInvokation modKey (from,to) = do
    fromLoc <- insertLoc modKey from
    toLoc <- insertLoc modKey to
    case (fromLoc, toLoc) of
      (Just (fromKey, _), Just (toKey, _)) -> void $ insert' always (HsInstanceInvokation modKey fromKey toKey)
      _                                    -> return ()

storeImport :: HasCallStack => Key HsModule -> ModuleName -> ParseM ()
storeImport importer modName = void $ do
  imported <- insertWithCache ifNotExist psModules (HsModule (showSDocUnsafe $ ppr modName) Nothing)
  void $ insertWithCache always psImports (HsImport importer imported)
