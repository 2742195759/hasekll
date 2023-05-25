{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import TCPHandle (Headers, ServerAddress(..), Content, serverMain, getInt, getString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8


addFunction :: Headers -> Content -> B.ByteString
addFunction headers content =
    let x = getInt headers ""
        y = getInt headers "y"
    {-in  C8.pack $ (++ "\n") . show $ x + y-}
    in  C8.pack "success."

serverAddress = ServerAddress "127.0.0.1" "10000"
main = serverMain serverAddress addFunction

