{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.CodeCompass.Schema
  ( module Development.CodeCompass.Schema
  , module Development.CodeCompass.Representation
  ) where

import Database.Persist.TH
import Database.Persist.Sqlite

import Development.CodeCompass.Representation

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
    type String Maybe
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
HsTag
    module HsModuleId
    name HsNameId
    tag Tag
    deriving Show
    deriving Eq
    deriving Ord
    UniqueTag name tag
HsInstanceInvokation
    module HsModuleId
    location HsSourceLocId
    invoked HsSourceLocId
    deriving Show
    deriving Eq
    deriving Ord
    UniqueInstanceInvokation location
HsComment
    module HsModuleId
    element HsSourceLocId
    comment String
    deriving Show
    deriving Eq
    deriving Ord
|]