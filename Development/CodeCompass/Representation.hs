{-# LANGUAGE TemplateHaskell            #-}
module Development.CodeCompass.Representation where

import Database.Persist.TH

data Tag = Instance | Context
    deriving (Show, Read, Eq, Ord)

derivePersistField "Tag"
