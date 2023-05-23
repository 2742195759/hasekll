module Main (main) where

import qualified Data.ByteString as B
import Data.String (fromString)
import Lib
import Buffer (PipelineT, consumeLine, execPipeline, BufferClass, appendBuffer, buf_fromString, peekBuffer, buf_contain_nl)
import           Network.Simple.TCP
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity (Identity, runIdentity)

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
            _ -> return ((fromString "")::B.ByteString)

main :: IO ()
main = do 
    putStrLn "Start Servering in 10000 port: "
    serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do
        {-maybe_msg <- recv connectionSocket -}
        line <- (execPipeline $ socketReadline connectionSocket)::IO (B.ByteString, B.ByteString)
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        print (line)
