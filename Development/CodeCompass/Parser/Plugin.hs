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

import ApiAnnotation
import Bag
import CoreSyn
import DynFlags
import FastString
import HscTypes
import HsDecls
import HsExpr
import HsExtension
import HsSyn hiding (HsModule)
import Id
import Lexer
import Module
import Name
import Outputable
import Parser
import Plugins
import SrcLoc
import StringBuffer
import TcRnTypes
import Type
import Var

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char (isSpace)
import Data.Function
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations
import Data.List (nubBy, sortOn, isPrefixOf, minimumBy, maximumBy)
import Data.Maybe
import Data.Time.Clock
import Database.Persist
import GHC.Stack
import TcEvidence

import Development.CodeCompass.Parser.Names
import Development.CodeCompass.Parser.Utils
import Development.CodeCompass.Schema as Schema

-- | Register the plugin 
plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsedAction
  , renamedResultAction = renamedAction
  , typeCheckResultAction = typecheckPlugin
  -- , spliceRunAction = spliceRun
  -- , interfaceLoadAction = interfaceLoad
  }

-- | Actions for the parsed stage:
-- Clean up the previously exported data for this module and modules depending on it
-- Store the record of the module
-- Store the comments in the module
parsedAction :: HasCallStack => [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction [] _ _
  = error "CodeCompass plugin needs a connection string to a database to store the data."
parsedAction arguments ms parse
  = liftIO $ connectDB arguments $ wrapErrorHandling (ms_mod ms) (lazyStore (ms_mod ms) (Right (ms_hs_date ms)) $ do 
      wrapTiming (ms_mod ms) "cleanUp" (void . insert)
        $ cleanUp (ms_mod ms)
      flip evalStateT initState $ do
        mod <- wrapTiming (ms_mod ms) "insertModule" insIn
                 $ insertModule (ms_mod ms) (hsmodName (unLoc $ hpm_module parse))
        comments <- wrapTiming (ms_mod ms) "parseForComments" insIn
                      $ liftIO $ parseForComments ms
        let withAttach = commentAttach comments
            moduleItself = (\n -> (getLoc n, getLoc n)) <$> hsmodName (unLoc $ hpm_module parse)
            attachableNames = map (\(n,sp) -> (getLoc n, sp)) $ hsGetNames noSrcSpan 
                           $ hsmodDecls $ unLoc $ hpm_module parse
            attachable = catMaybes [moduleItself] ++ attachableNames
        wrapTiming (ms_mod ms) "attach" insIn
          $ mapM_ (attach mod attachable) withAttach
        wrapTiming (ms_mod ms) "storeModuleName" insIn
          $ mapM_ (storeModuleName mod) (universeBi (hpm_module parse)))
      (return parse)

-- | Store a 'HsName' corresponding to the module's name.
storeModuleName :: HasCallStack => Key HsModule -> Located ModuleName -> ParseM ()
storeModuleName mod (L l modName) = do 
  store <- insertLoc mod l
  findMod <- lift $ selectList [HsModuleModuleName ==. moduleNameString modName] []
  case (store, findMod) of
    (Just (loc, _), (entityVal -> HsModule _ modNameLoc):_) 
      -> void $ insert' ifNotExist $ HsName mod (moduleNameString modName) loc modNameLoc Nothing Nothing
    _ -> return ()

-- | Find the element where the comment is attached and store the connection in the database.
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

-- | Join consecutive comments and decide where they belong
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

-- | Get the text from an 'AnnotationComment'
getCommentString :: AnnotationComment -> String
getCommentString (AnnLineComment str) = str
getCommentString (AnnBlockComment str) = str

-- | Api comment attach direction
data AttachRule = AttachNext | AttachPrevious
  deriving Show

-- | Decide where the comments will be attached (next element or previous)
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

-- | Re-parse the module to record comments (currently we need this because we
-- cannot set 'Opt_KeepRawTokenStream' for the original parse)
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

-- | Record the current module in the database
insertModule :: HasCallStack => Module -> Maybe (Located ModuleName) -> ParseM (Key HsModule)
insertModule m Nothing = insert' always $ HsModule (showSDocUnsafe (ppr m)) Nothing
insertModule m (Just name) = do 
  modKey <- insert' always $ HsModule (showSDocUnsafe (ppr m)) Nothing
  loc <- insertLoc modKey (getLoc name)
  case loc of Just (l,_) -> lift $ update modKey [HsModuleModNameLoc =. Just l]
              Nothing -> return ()
  return modKey

-- | Perform actions on the named representation
-- Store names and the location of their definitions.
-- Store 'Instance' and 'Context' tags for names that refer to type classes
-- based on their location.
renamedAction :: HasCallStack => [CommandLineOption]
                    -> TcGblEnv -> HsGroup GhcRn
                    -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction arguments tc gr
  = liftIO $ connectDB arguments $ wrapErrorHandling (tcg_mod tc) (lazyStore (tcg_mod tc) (Left sourceName) $ void $ do
       let names = universeBi gr
           lieNames = universeBi (tcg_rn_exports tc)
                        ++ universeBi (tcg_rn_imports tc) :: [Located Name]
           sortedNames = nubBy ((==) `on` getLoc)
                           $ filter (isGoodSrcSpan . getLoc)
                           $ sortOn getLoc (names ++ lieNames)
       evalStateT (do [mod] <- lift $ selectKeysList [HsModuleModuleName ==. showSDocUnsafe (ppr (tcg_mod tc))] []
                      let instSections = catMaybes $ map (fmap (,Instance) . instanceSections)
                                                         (concatMap group_instds $ hs_tyclds gr)
                          contextSections = concatMap (map (,Context) . getContext) (universeBi gr :: [LHsType GhcRn])
                          sections = instSections ++ contextSections
                      wrapTiming (tcg_mod tc) "storeName" insIn
                        $ mapM_ (storeName sections mod) sortedNames
                  ) initState)
       (return (tc, gr))
  where sourceName = unpackFS $ srcSpanFile $ tcg_top_loc tc

-- | Get a location where class names are used as an 'Instance'
instanceSections :: HasCallStack => Located (InstDecl a) -> Maybe RealSrcSpan
instanceSections (L _ (ClsInstD _ inst)) = maybeRealSrcSpan $ findTypeFunLoc (hsib_body $ cid_poly_ty inst)
instanceSections (L _ (DataFamInstD _ inst)) = maybeRealSrcSpan $ getLoc (feqn_tycon $ hsib_body $ dfid_eqn inst)
instanceSections _ = Nothing

-- | Get the location of the class name in an instance head
findTypeFunLoc :: HasCallStack => Located (HsType a) -> SrcSpan
findTypeFunLoc (L _ (HsAppTy _ t _)) = findTypeFunLoc t
findTypeFunLoc (L _ (HsOpTy _ _ (L l _) _)) = l
findTypeFunLoc (L _ (HsParTy _ t)) = findTypeFunLoc t
findTypeFunLoc (L l _) = l

-- | Get the predicates of a type
getContext :: HasCallStack => Located (HsType a) -> [RealSrcSpan]
getContext (L l (HsQualTy _ ctx _)) = catMaybes $ map (maybeRealSrcSpan . findTypeFunLoc) (unLoc ctx)
getContext _ = []

-- | SrcSpan to Maybe RealSrcSpan
maybeRealSrcSpan :: HasCallStack => SrcSpan -> Maybe RealSrcSpan
maybeRealSrcSpan (RealSrcSpan sp) = Just sp
maybeRealSrcSpan _ = Nothing

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

-- | Look up tags for a name (which have ranges containing the range of the name)
lookupSection :: [(RealSrcSpan,Tag)] -> SrcSpan -> [Tag]
lookupSection sections (RealSrcSpan sp) = map snd $ filter (\(s,_) -> s `containsSpan` sp) sections

-- | Insert a source location into the database
insertLoc :: HasCallStack => Key HsModule -> SrcSpan -> ParseM (Maybe (Key HsSourceLoc, Key HsFile))
insertLoc mod (RealSrcSpan rsp) = do
  file <- insertFile mod (unpackFS (srcSpanFile rsp))
  sl <- insertWithCache psSourceLoc (srcSpanToLoc mod file rsp)
  return $ Just (sl,file)
insertLoc _ _ = return Nothing

-- | Create a database record from the source location
srcSpanToLoc :: HasCallStack => Key HsModule -> Key HsFile -> RealSrcSpan -> HsSourceLoc
srcSpanToLoc mod file rsp 
  = HsSourceLoc mod file (srcSpanStartLine rsp) (srcSpanStartCol rsp) 
                         (srcSpanEndLine rsp) (srcSpanEndCol rsp)

-- | Insert the record of a file into the database
insertFile :: HasCallStack => Key HsModule -> String -> ParseM (Key HsFile)
insertFile mod str = insertWithCache psFile (HsFile str mod)

-- | Operations on the typed representation
-- Store the type of the names
-- Store the actual types of the names
-- Store imports of other modules
-- Determine which type class instances are invoked in class function calls
-- Records that the export have been successful
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

-- | 'typecheckPlugin' function without the wrappings
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
        -- | Find out if there is an instance invokation in the expression and give back the target
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

        exprName :: HasCallStack => LHsExpr GhcTc -> Maybe (Located Id)
        exprName (L l (HsWrap _ _ w)) = exprName (L l w)
        exprName (L l (HsVar _ id)) = Just $ L l $ unLoc id
        exprName _ = Nothing

-- | Store the type (both declared and concrete) at the name invocation.
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

-- | Store the instance invokation
storeInstanceInvokation :: HasCallStack => Key HsModule -> (SrcSpan, SrcSpan) -> ParseM ()
storeInstanceInvokation modKey (from,to) = do
    fromLoc <- insertLoc modKey from
    toLoc <- insertLoc modKey to
    case (fromLoc, toLoc) of
      (Just (fromKey, _), Just (toKey, _)) -> void $ insert' always (HsInstanceInvokation modKey fromKey toKey)
      _                                    -> return ()

-- | Store the module import
storeImport :: HasCallStack => Key HsModule -> ModuleName -> ParseM ()
storeImport importer modName = void $ do
  imported <- insertWithCache psModules (HsModule (showSDocUnsafe $ ppr modName) Nothing)
  void $ insertWithCache psImports (HsImport importer imported)
