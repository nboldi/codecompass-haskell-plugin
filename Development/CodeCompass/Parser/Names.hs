{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}


module Development.CodeCompass.Parser.Names where

import Data.Generics.Uniplate.Data ()
import Data.List

import Bag (Bag, bagToList, unionManyBags)
import BasicTypes (SourceText(..))
import ConLike (ConLike(..))
import Data.Maybe (Maybe(..), listToMaybe)
import GHC
import Id (Id, mkVanillaGlobal)
import OccName (OccName)
import Outputable (Outputable(..), showSDocUnsafe)
import PatSyn (patSynSig)
import RdrName (RdrName, rdrNameOcc, nameRdrName)
import SrcLoc
import Type (TyThing(..), mkFunTys)

-- | Get names from the GHC AST
class HsHasName a where
  hsGetNames :: SrcSpan -> a -> [(Located RdrName, SrcSpan)]

instance HsHasName e => HsHasName [e] where
  hsGetNames sp es = concatMap (hsGetNames sp) es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames _ (L l e) = hsGetNames l e

instance HsHasName (HsLocalBinds GhcPs) where
  hsGetNames sp (HsValBinds _ bnds) = hsGetNames sp bnds
  hsGetNames _ _ = []

instance HsHasName (HsDecl GhcPs) where
  hsGetNames sp (TyClD _ tycl) = hsGetNames sp tycl
  hsGetNames sp (ValD _ vald) = hsGetNames sp vald
  hsGetNames sp (SigD _ sig) = hsGetNames sp sig
  hsGetNames sp (ForD _ ford) = hsGetNames sp ford
  hsGetNames sp (InstD _ inst) = hsGetNames sp inst
  hsGetNames _ _ = []

instance HsHasName (InstDecl GhcPs) where
  hsGetNames sp (ClsInstD _ clsInst) = hsGetNames sp (cid_datafam_insts clsInst)
  hsGetNames sp (DataFamInstD _ dataFamInst) = hsGetNames sp dataFamInst
  hsGetNames _ _ = []

instance (HsHasName r) => HsHasName (FamEqn GhcPs p r) where
  hsGetNames sp (FamEqn _ id _ _ rhs) = (id,sp) : hsGetNames sp rhs

instance HsHasName (DataFamInstDecl GhcPs) where
  hsGetNames sp dfid = hsGetNames sp (hsib_body $ dfid_eqn dfid)

instance HsHasName (TyClGroup GhcPs) where
  hsGetNames sp (TyClGroup _ tycls _ _) = hsGetNames sp tycls

instance HsHasName (TyClDecl GhcPs) where
  hsGetNames sp (FamDecl _ fd) = hsGetNames sp fd
  hsGetNames sp (SynDecl {tcdLName = name}) = [(name, sp)]
  hsGetNames sp (DataDecl {tcdLName = name, tcdDataDefn = datadef})
    = [(name, sp)] ++ hsGetNames sp datadef
  hsGetNames sp (ClassDecl {tcdLName = name, tcdSigs = sigs, tcdATs = typeAssocs})
    = [(name, sp)] ++ hsGetNames sp sigs ++ hsGetNames sp typeAssocs

instance HsHasName (FamilyDecl GhcPs) where
 hsGetNames sp (FamilyDecl { fdLName = name }) = [(name, sp)]

instance HsHasName (HsDataDefn GhcPs) where
  hsGetNames sp (HsDataDefn {dd_cons = ctors}) = hsGetNames sp ctors

instance HsHasName (ConDecl GhcPs) where
  hsGetNames sp (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsFunTy _ (L _ (HsRecTy _ flds)) _))})
    = map (,sp) names ++ hsGetNames sp flds
  hsGetNames sp (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsRecTy _ flds))})
    = map (,sp) names ++ hsGetNames sp flds
  hsGetNames sp (ConDeclGADT {con_names = names}) = map (,sp) names
  hsGetNames sp (ConDeclH98 {con_name = name, con_args = details})
    = [(name, sp)] ++ hsGetNames sp details

instance HsHasName (HsConDeclDetails GhcPs) where
  hsGetNames sp (RecCon rec) = hsGetNames sp rec
  hsGetNames _ _ = []

instance HsHasName (ConDeclField GhcPs) where
  hsGetNames sp (ConDeclField _ name _ _) = hsGetNames sp name

instance HsHasName (FieldOcc GhcPs) where
  hsGetNames sp (FieldOcc _ name) = [(name, sp)]


instance HsHasName (Sig GhcPs) where
  hsGetNames sp (TypeSig _ n _) = map (,sp) n
  hsGetNames sp (ClassOpSig _ _ n _) = map (,sp) n
  hsGetNames sp (PatSynSig _ n _) = map (,sp) n
  hsGetNames _ _ = []

instance HsHasName (ForeignDecl GhcPs) where
  hsGetNames sp (ForeignImport _ name _ _) = [(name, sp)]
  hsGetNames _ _ = []

instance HsHasName (HsValBinds GhcPs) where
  hsGetNames sp (ValBinds _ bnds sigs)
    = hsGetNames sp bnds ++ hsGetNames sp sigs
  hsGetNames sp _ = []

instance HsHasName e => HsHasName (Bag e) where
  hsGetNames sp = hsGetNames sp . bagToList

instance HsHasName (HsBind GhcPs) where
  hsGetNames sp (FunBind {fun_id = lname}) = [(lname, sp)]
  hsGetNames sp (PatBind {pat_lhs = pat}) = hsGetNames sp pat
  hsGetNames sp (PatSynBind _ (PSB {psb_id = name})) = [(name, sp)]
  hsGetNames _ _ = error "hsGetNames: called on compiler-generated binding"

instance HsHasName (ParStmtBlock l GhcPs) where
  hsGetNames _ (ParStmtBlock _ _ binds _) = [] -- hsGetNames binds

-- instance HsHasName (Sig GhcPs) where
--   hsGetNames _ (ParStmtBlock _ _ binds _) = [] -- hsGetNames binds


-- instance HsHasName (LHsTyVarBndrs GhcPs) where
--   hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

instance HsHasName (HsTyVarBndr GhcPs) where
  hsGetNames sp (UserTyVar _ name) = [(name, sp)]
  hsGetNames sp (KindedTyVar _ name _) = [(name, sp)]
  hsGetNames _ _ = []

instance HsHasName (Match GhcPs b) where
  hsGetNames sp (Match _ _ pats _) = concatMap (hsGetNames sp) pats

instance HsHasName (StmtLR GhcPs GhcPs b) where
  hsGetNames sp (LetStmt _ binds) = hsGetNames sp binds
  hsGetNames sp (BindStmt _ pat _ _ _) = hsGetNames sp pat
  hsGetNames _ _ = []

instance HsHasName (Pat GhcPs) where
  hsGetNames sp (VarPat _ name) = [(name, sp)]
  hsGetNames sp (LazyPat _ p) = hsGetNames sp p
  hsGetNames sp (AsPat _ name p) = [(name, sp)] ++ hsGetNames sp p
  hsGetNames sp (ParPat _ p) = hsGetNames sp p
  hsGetNames sp (BangPat _ p) = hsGetNames sp p
  hsGetNames sp (ListPat _ pats) = concatMap (hsGetNames sp) pats
  hsGetNames sp (TuplePat _ pats _) = concatMap (hsGetNames sp) pats
  hsGetNames sp (ConPatIn _ details) = concatMap (hsGetNames sp) (hsConPatArgs details)
  hsGetNames sp (ConPatOut {pat_args = details}) = concatMap (hsGetNames sp) (hsConPatArgs details)
  hsGetNames sp (ViewPat _ _ p) = hsGetNames sp p
  hsGetNames sp (NPlusKPat _ name _ _ _ _) = [(name, sp)]
  hsGetNames sp (SigPat _ p) = hsGetNames sp p
  hsGetNames _ _ = []

instance HsHasName (HsGroup GhcPs) where
  hsGetNames sp g@(HsGroup _ vals _ clds _ _ _ foreigns _ _ _ _)
    = hsGetNames sp vals ++ hsGetNames sp clds
        ++ hsGetNames sp (hsGroupInstDecls g) ++ hsGetNames sp foreigns
