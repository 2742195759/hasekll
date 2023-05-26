{-# LANGUAGE OverloadedStrings #-}
module TCPHandle (
    Headers,
    ServerAddress(..), 
    Content, 
    serverMain,
    getInt,
    getString,
    TCPHandle
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
import Debug.Trace

--- Monad Transformer Learning by practice

socketReadline :: Socket -> PipelineT B.ByteString IO B.ByteString
socketReadline sock = do 
    buf <- peekBuffer 
    if buf_contain_nl buf then consumeLine
    else do 
        string <- lift $ tcpRead sock
        appendBuffer   string
        socketReadline sock

type Headers = Map.Map B.ByteString B.ByteString

httpReadFirstLine :: Socket -> PipelineT B.ByteString IO Headers
httpReadFirstLine sock = do 
    line <- socketReadline sock
    let is_space x = x == c2w ' '
    let (method, remain) = B.break is_space line
    let (host, protocal) = B.break is_space (B.tail remain)
    return (Map.fromList [("method",method), ("host", host), ("protocal", protocal)]:: Headers)

httpReadHeader :: Socket -> PipelineT B.ByteString IO Headers
httpReadHeader sock = do 
    line <- socketReadline sock
    if line == B.empty
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


httpParserRequest  :: Socket -> PipelineT B.ByteString IO Headers
httpParserRequest sock = do 
    headers1 <- httpReadFirstLine sock
    headers2 <- httpReadHeader sock
    return $ Map.union headers1 headers2


tcpRead :: Socket -> IO B.ByteString
tcpRead sock = do 
    maybe_string <- recv sock 1000
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
getInt header = read . getString header


getString :: Headers -> B.ByteString -> String
getString headers name = 
    let x = Map.lookup name headers 
    in case x of 
        Just x -> C8.unpack $ C8.strip x
        _ -> throw $ AssertionFailed $ "can't found key with " ++ C8.unpack name

type TCPHandle = Headers -> Content -> IO B.ByteString

ioWrapper :: (Headers -> Content -> B.ByteString) -> TCPHandle
ioWrapper func headers content = return (func headers content) 

serverMain :: ServerAddress -> (Headers -> Content -> IO B.ByteString) -> IO ()
serverMain address handle = do 
    print ("Start Servering in ", port address) 
    serve (Host $ ip address) (port address) $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        (remains, headers) <- (execPipeline $ httpParserRequest connectionSocket)::IO (B.ByteString, Headers)
        let maybe_content_length = Map.lookup (fromString "Content-Length"::B.ByteString) headers
        let content_length = case maybe_content_length of 
                                Just content -> byteString2Int content
                                _            -> 0
        (_, content) <- execPipeline $ do  
            resetBuffer remains
            httpReadNumber connectionSocket content_length
        result <- handle headers content
        let len = C8.length result
        send connectionSocket $ C8.pack "HTTP/1.1 200 OK\n\r"
        send connectionSocket $ C8.pack ("Content-Length: " ++ show len ++ "\n\r\n\r")
        send connectionSocket result
