{-# LANGUAGE OverloadedStrings #-}
module TCPHandle (
    Headers,
    ServerAddress(..), 
    Content, 
    serverMain,
    getInt
) where

import qualified Data.ByteString as B
import Data.String (fromString)
import Buffer (PipelineT, consumeLine, execPipeline, appendBuffer, peekBuffer, buf_contain_nl, resetBuffer)
import Network.Simple.TCP
import Control.Monad.Trans.Class
import Data.ByteString.Internal (c2w)
import qualified Data.Map.Strict as Map 
import Control.Exception (throw, Exception, AssertionFailed(..))
import qualified Data.ByteString.Char8 as C8

--- Monad Transformer Learning by practice

socketReadline :: Socket -> PipelineT B.ByteString IO B.ByteString
socketReadline sock = do 
    buf <- peekBuffer 
    if buf_contain_nl buf then consumeLine
    else do 
         maybe_string <- lift $ recv sock 100
         case maybe_string of 
            Just string -> do 
                appendBuffer   string
                socketReadline sock
            _ -> return (fromString "closed"::B.ByteString)

type Headers = Map.Map B.ByteString B.ByteString

httpReadHeader :: Socket -> PipelineT B.ByteString IO Headers
httpReadHeader sock = do 
    line <- socketReadline sock
    if line == (fromString ""::B.ByteString) 
    then return Map.empty
    else do
        headers <- httpReadHeader sock
        let is_colon x = x == c2w ':'
        let (k, v) = B.break is_colon line
        return $ Map.insert k (B.tail v) headers 

httpReadNumber :: Socket -> Int -> PipelineT B.ByteString IO B.ByteString
httpReadNumber sock num = 
    if num == 0 then return C8.empty
    else do
        buf <- peekBuffer
        let len = C8.length buf 
        if len >= num 
        then do 
            resetBuffer (C8.takeEnd (len - num) buf)
            return $ C8.take num buf
        else do
            string <- lift $ tcpRead sock
            appendBuffer   string
            httpReadNumber sock num

tcpRead :: Socket -> IO B.ByteString
tcpRead sock = do 
    maybe_string <- recv sock 100 
    case maybe_string of
        Just string -> return string
        _ -> throw $ MyException "unexpected closed."

type Content = B.ByteString

byteString2Int :: B.ByteString -> Int
byteString2Int = read . C8.unpack


newtype MyException = MyException String deriving Show
instance Exception MyException 
data ServerAddress = ServerAddress { ip::String, port::String }


getInt :: Headers -> B.ByteString -> Int
getInt headers name = 
    let x = Map.lookup name headers 
        asInt = read . C8.unpack
    in case x of 
        Just x -> asInt x
        _ -> throw $ AssertionFailed $ "can't found key with " ++ (C8.unpack name)


serverMain :: ServerAddress -> (Headers -> Content -> B.ByteString) -> IO ()
serverMain address handle = do 
    print ("Start Servering in ", port address) 
    serve (Host $ ip address) (port address) $ \(connectionSocket, remoteAddr) -> do
        {-maybe_msg <- recv connectionSocket -}
        (remains, headers) <- (execPipeline $ httpReadHeader connectionSocket)::IO (B.ByteString, Headers)
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        let maybe_content_length = Map.lookup (fromString "Content-Length"::B.ByteString) headers
        let content_length = case maybe_content_length of 
                                Just content -> byteString2Int content
                                _            -> throw $ MyException "Content-Length Missing."
        (_, content) <- execPipeline $ do  
            resetBuffer remains
            httpReadNumber connectionSocket content_length
        let result = handle headers content
        send connectionSocket result
