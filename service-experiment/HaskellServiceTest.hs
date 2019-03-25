{-# LANGUAGE OverloadedStrings #-}

import qualified LanguageService_Client as Client
import Language_Types
import Common_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Server

import Control.Exception
import Data.Maybe
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as Map
import Data.Text.Lazy
import Text.Printf
import Network

main = do
  transport  <- hOpen ("localhost" :: String, PortNumber 9090)
  let binProto = BinaryProtocol transport
  let client = (binProto, binProto)

  fileTypes <- Client.getFileTypes client
  print $ "getFileTypes: " ++ show fileTypes
  
  nodeInfo <- Client.getAstNodeInfoByPosition client (FilePosition "/home/nboldi/myhaskellproject/Test.hs" (Position 8 5))
  print $ "getAstNodeInfoByPosition: " ++ show nodeInfo

  refTypes <- Client.getReferenceTypes client (astNodeInfo_id nodeInfo)
  print $ "getReferenceTypes: " ++ show refTypes
  
  refs <- Client.getReferences client (astNodeInfo_id nodeInfo) (fromJust $ Map.lookup "Name" refTypes) Vector.empty
  print $ "getReferences: " ++ show refs
  
  
  

