{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main (main) where

import TCPHandle (Headers, ServerAddress(..), Content, serverMain, getInt, getString, TCPHandle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Directory (listDirectory)
import System.IO (FilePath)
import Debug.Trace
import Control.Exception (AssertionFailed(AssertionFailed))
import Control.Exception
import Data.Aeson
import GHC.Generics


data PathRequest = PathRequest  {
      path     :: String
    , command  :: String
    , content  :: String
    } deriving (Generic, Show)

getReqContent :: PathRequest -> String
getReqContent (PathRequest p a b) = b

data PathRespond = EmptyRespond { message :: String }
     | ContentRespond { message::String , text :: String }
     | FilesRespond { message::String,  files::[String] }
    deriving (Generic, Show)


instance ToJSON PathRespond 
instance FromJSON PathRespond 
instance ToJSON PathRequest  
instance FromJSON PathRequest


pathFinder :: PathRequest -> IO PathRespond
pathFinder request = 
    let _path = path request
        files = fmap (_path ++) <$> listDirectory _path
    in FilesRespond "success" <$> files


fetchFile :: PathRequest -> IO PathRespond
fetchFile request = 
    let _path = path request
        contents = readFile _path
    in ContentRespond "success" <$> contents


storeFile :: PathRequest -> IO PathRespond
storeFile request = 
    let _path = path request
        content_str = getReqContent request
    in  writeFile _path content_str >> 
        (return $ EmptyRespond "success")


showRequest :: PathRequest -> IO PathRespond
showRequest request = 
        print request >> 
        (return $ EmptyRespond "success")

getRespond :: String -> PathRequest -> IO PathRespond
getRespond cmd req = case cmd of 
        "ls" ->   pathFinder req
        "fetch" -> fetchFile req
        "store" -> storeFile req
        "show"  -> showRequest req
        _     -> throw $ AssertionFailed "non-support command."
    

dispatcher :: TCPHandle
dispatcher headers content = 
    let req = decode $ B.fromStrict content :: Maybe PathRequest
        maybe_cmd = do command <$> req
        maybe_response =  case maybe_cmd of 
            Just x     ->  (getRespond x) <$> req
            _  -> throw $ AssertionFailed "decode error."
    in  case maybe_response of 
            Just response -> B.toStrict . encode <$> response
            _ -> throw $ AssertionFailed "decode error."


ioWrapper :: (Headers -> Content -> B.ByteString) -> TCPHandle
ioWrapper func headers content = return (func headers content) 


serverAddress = ServerAddress "127.0.0.1" "10000"
main = serverMain serverAddress dispatcher

