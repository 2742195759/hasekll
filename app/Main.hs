{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Lib

{-dependencies is: network-simple-}
import           Network.Simple.TCP
import qualified Data.ByteString as B
{-import           Conduit-}
import Control.Monad

{-recvAll :: Socket -> IO (Maybe B.ByteString)-}
{-recvAll connectionSocket = do -}
    {-maybe_bytes <- recv connectionSocket 1000-}
    {-case maybe_bytes  of -}
        {-Nothing     -> return Nothing-}
        {-Just bytes  -> do -}
            {-maybe_others <- recvAll connectionSocket -}
            {-case maybe_others  of -}
                {-Nothing -> return $ Just $ B.empty-}
                {-Just others -> return (Just $ B.concat [bytes, others])-}

{-[>Read a row of data from it, If we encounter the end of a line, stop directly <]-}
{-readline  :: Socket -> IO B.ByteString-}
{-readline connectionSocket = do -}
    {-maybe_bytes <- recv connectionSocket size-}
    {-case maybe_bytes  of -}
        {-Nothing     -> return Nothing-}
        {-Just bytes  -> do -}
            {-maybe_others <- recvAll connectionSocket -}
            {-case maybe_others  of -}
                {-Nothing -> return $ Just $ B.empty-}
                {-Just others -> return (Just $ B.concat [bytes, others])-}

type Buffer = String
data Pipeline a = Pipeline { runPipeline :: Buffer -> (Buffer, a) }


instance Functor Pipeline
instance Applicative Pipeline
instance Monad Pipeline where
    return x = Pipeline (\b -> (b, x))
    (>>=) cx f = Pipeline $ \buf -> 
                    let (oldbuf, oldvalue) = runPipeline cx buf
                    in runPipeline (f oldvalue) oldbuf


execPipeline :: Buffer -> Pipeline a -> (Buffer, a)
execPipeline buf pipe = runPipeline pipe buf
                    
setBuffer :: Buffer -> Pipeline ()
setBuffer x = Pipeline fn where 
                fn buffer = (x, ())

consumeLine :: Pipeline String
consumeLine = Pipeline func where
                    func buf = (tail y, x) where
                        (x, y) = break is_nl buf where 
                            is_nl c = c == '\n'

appendBuffer :: Buffer -> Pipeline ()
appendBuffer x = Pipeline fn where 
                fn buffer = (buffer ++ x, ())

parser :: Pipeline String
parser = do 
    setBuffer "sdfsdfsdf\nsdfsd\n\n"
    appendBuffer "xkxkx\n"
    consumeLine
    consumeLine
    consumeLine

main :: IO ()
main = do 
    {-putStrLn "Start Servering in 10000 port: "-}
    {-serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do-}
        {-[>maybe_msg <- recv connectionSocket <]-}
        {-putStrLn $ "TCP connection established from " ++ show remoteAddr-}
    let (buf, val) = execPipeline "" parser
    print ((buf, val))
