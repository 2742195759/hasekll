{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import TCPHandle (Headers, ServerAddress(..), Content, serverMain, getInt)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import Control.Exception (throw, AssertionFailed(..))


addFunction :: Headers -> Content -> B.ByteString
addFunction headers content =
    let x = getInt headers "x"
        y = getInt headers "y"
    in  C8.pack $ (++ "\n") . show $ x + y


serverAddress = ServerAddress "127.0.0.1" "10000"
main = serverMain serverAddress addFunction
