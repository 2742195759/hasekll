{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import TCPHandle (Headers, ServerAddress(..), Content, serverMain, getInt, getString, TCPHandle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Directory (listDirectory)
import System.IO (FilePath)
import Debug.Trace
import Control.Exception (AssertionFailed(AssertionFailed))
import Control.Exception


pathFinder :: TCPHandle
pathFinder headers _ = 
    let path = getString headers "path" 
        files = fmap (path ++) <$> listDirectory path
        unline_files = unlines <$> files
    in C8.pack <$> unline_files


getFile :: TCPHandle
getFile headers _ = 
    let path = getString headers "path" 
        files = readFile path
    in C8.pack <$> files


dispatcher :: TCPHandle
dispatcher headers = 
    let command = getString headers "command"
    in case command of 
        "ls" -> pathFinder headers
        "cat"-> getFile headers
        _    -> throw $ AssertionFailed "non-support command."


ioWrapper :: (Headers -> Content -> B.ByteString) -> TCPHandle
ioWrapper func headers content = return (func headers content) 


serverAddress = ServerAddress "127.0.0.1" "10000"
main = serverMain serverAddress dispatcher

