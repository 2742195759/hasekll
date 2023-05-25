{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}


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

data EmptyRespond = EmptyRespond {
      message :: String
    } deriving (Generic, Show)

data ContentRespond = ContentRespond {
      message  :: String
    , content  :: String
    } deriving (Generic, Show)

data FilesRespond = FilesRespond {
      message  :: String
    , filelist :: [String]
    } deriving (Generic, Show)


instance ToJSON EmptyRespond 
instance FromJSON EmptyRespond 
instance ToJSON ContentRespond 
instance FromJSON ContentRespond 
instance ToJSON FilesRespond 
instance FromJSON FilesRespond 
instance ToJSON PathRequest  
instance FromJSON PathRequest


pathFinder :: PathRequest -> IO FilesRespond
pathFinder request = 
    let _path = path request
        files = fmap (_path ++) <$> listDirectory _path
    in (FilesRespond "success") <$> files


fetchFile :: PathRequest -> IO ContentRespond
fetchFile request = 
    let _path = path request
        contents = readFile _path
    in (ContentRespond "success") <$> contents


storeFile :: PathRequest -> IO EmptyRespond
storeFile request = 
    let _path = path request
        content_str = getReqContent request
    in  writeFile _path content_str >> 
        return $ EmptyRespond "success"


showRequest :: PathRequest -> IO EmptyRespond
showRequest request = 
    print (request) >> 
    return $ EmptyRespond "success"

getRespond :: (ToJSON a) => String -> PathRequest -> IO a
getRespond cmd req = case cmd of 
        "ls" ->   pathFinder req
        _     -> throw $ AssertionFailed "non-support command."
    

dispatcher :: TCPHandle
dispatcher headers content = 
    let req = decode $ B.fromStrict content :: Maybe PathRequest
        maybe_cmd = do command <$> req
        response =  case maybe_cmd of 
            Just x     ->  getRespond x req
            _  -> throw $ AssertionFailed "decode error."
    in  (B.toStrict . encode) <$> response


ioWrapper :: (Headers -> Content -> B.ByteString) -> TCPHandle
ioWrapper func headers content = return (func headers content) 


serverAddress = ServerAddress "127.0.0.1" "10000"
main = serverMain serverAddress dispatcher

