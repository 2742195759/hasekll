module Main (main) where

import qualified Data.ByteString as B
import Data.String (fromString)
import Buffer (PipelineT, consumeLine, execPipeline, appendBuffer, peekBuffer, buf_contain_nl)
import Network.Simple.TCP
import Control.Monad.Trans.Class
import Data.ByteString.Internal (c2w)

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

data Headers = Headers { keys :: [B.ByteString], values :: [B.ByteString] } deriving Show

appendHeaders :: Headers -> B.ByteString -> B.ByteString -> Headers
appendHeaders headers key val = Headers (key:keys headers) (val:values headers)

httpReadHeader :: Socket -> PipelineT B.ByteString IO Headers
httpReadHeader sock = do 
    line <- socketReadline sock
    if line == (fromString ""::B.ByteString) 
    then return $ Headers [] []
    else do
        headers <- httpReadHeader sock
        let is_colon x = x == c2w ':'
        let (k, v) = B.break is_colon line
        return $ appendHeaders headers k v

main :: IO ()
main = do 
    putStrLn "Start Servering in 10000 port: "
    serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do
        {-maybe_msg <- recv connectionSocket -}
        (_, headers) <- (execPipeline $ httpReadHeader connectionSocket)::IO (B.ByteString, Headers)
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        print headers
