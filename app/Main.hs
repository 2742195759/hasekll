module Main (main) where

import qualified Data.ByteString as B
import Data.String (fromString)
import Buffer (PipelineT, consumeLine, execPipeline, appendBuffer, peekBuffer, buf_contain_nl)
import Network.Simple.TCP
import Control.Monad.Trans.Class
import Data.ByteString.Internal (c2w)
import qualified Data.Map.Strict as Map 
import Control.Exception (throw, Exception)
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
    then return $ Map.empty
    else do
        headers <- httpReadHeader sock
        let is_colon x = x == c2w ':'
        let (k, v) = B.break is_colon line
        return $ Map.insert k (B.tail v) headers 

httpReadNumber :: Socket -> Int -> PipelineT B.ByteString IO B.ByteString
httpReadNumber sock num = 
    if num == 0 then return empty
    else do
        buf <- peekBuffer
        let len = length buf 
        in if length buf >= num 
           then do 
              resetBuffer (C8.takeEnd 

type Content = B.ByteString
handleFunction :: Headers -> Content -> IO ()
handleFunction headers content = do 
    print (headers, content)

byteString2Int :: B.ByteString -> Int
byteString2Int = read . C8.unpack

data MyException = MyException String deriving Show
instance Exception MyException 

main :: IO ()
main = do 
    putStrLn "Start Servering in 10000 port: "
    serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do
        {-maybe_msg <- recv connectionSocket -}
        (remains, headers) <- (execPipeline $ httpReadHeader connectionSocket)::IO (B.ByteString, Headers)
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        let maybe_content_length = Map.lookup (fromString "Content-Length"::B.ByteString) headers
        let content_length = case maybe_content_length of 
                                Just content -> byteString2Int content
                                _            -> throw $ MyException "Content-Length Missing."
        print ("Remain:", remains)
        print (content_length)
        maybe_content <- recv connectionSocket content_length
        case maybe_content of 
            Just content -> handleFunction headers content
            _ -> throw $ MyException "content is less than content-length."
