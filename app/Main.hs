{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Lib

{-dependencies is: network-simple-}
import           Network.Simple.TCP
import qualified Data.ByteString as B
import Data.String (fromString)
import Data.Char (ord)
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

class BufferClass a where 
    buf_fromString :: String -> a
    buf_breakNL :: a -> (a, a)
    buf_tail :: a -> a
    buf_append :: a -> a -> a
    buf_contain_nl :: a -> Bool

instance BufferClass B.ByteString where 
    buf_fromString = fromString
    buf_breakNL = B.break is_nl where 
                is_nl c = c == (fromIntegral . ord) '\n'
    buf_contain_nl = B.elem nl where
                nl = (fromIntegral . ord) '\n'
    buf_tail = B.tail 
    buf_append = B.append

instance BufferClass String where 
    buf_fromString = id
    buf_breakNL = break is_nl where 
                is_nl c = c == '\n'
    buf_contain_nl x = '\n' `elem` x
    buf_tail = Prelude.tail
    buf_append = (++)

type Buffer = String
newtype Pipeline b a = Pipeline { runPipeline :: b -> (b, a) }

instance Functor (Pipeline b)
instance Applicative (Pipeline b)
instance Monad (Pipeline b) where
    return x = Pipeline $ \m -> (m, x)
    (>>=) cx f = Pipeline $ \buf -> 
                    let (oldbuf, oldvalue) = runPipeline cx buf
                    in runPipeline (f oldvalue) oldbuf


execPipeline :: (BufferClass b) => b -> Pipeline b a -> (b, a)
execPipeline buf pipe = runPipeline pipe buf
                    
resetBuffer :: b -> Pipeline b ()
resetBuffer x = Pipeline fn where 
                fn _ = (x, ())

readFromSockets :: (BufferClass b) => Pipeline b () 
readFromSockets = appendBuffer $ buf_fromString "sdfsdf\n"

consumeLine :: (BufferClass b) => Pipeline b b
consumeLine = Pipeline func where
                func buf = (buf_tail y, x) where 
                   (x, y) = buf_breakNL buf

consumeWithRead :: (BufferClass b) => Pipeline b b 
consumeWithRead = do 
    buf <- peekBuffer 
    if buf_contain_nl buf then consumeLine
    else do 
        readFromSockets
        consumeLine


appendBuffer :: (BufferClass b) => b -> Pipeline b ()
appendBuffer x = Pipeline fn where 
                fn buffer = (buf_append buffer x, ())


peekBuffer :: Pipeline b b
peekBuffer = Pipeline $ \buf -> 
                (buf, buf)

parser :: (BufferClass b) => Pipeline b [b]
parser = do 
    resetBuffer $ buf_fromString "sdfsdfsdf\nsdfsd\n\n"
    appendBuffer $ buf_fromString "xkxkx\n"
    appendBuffer $ buf_fromString "half line "
    a <- consumeLine
    b <- consumeLine
    c <- consumeLine
    d <- consumeLine
    e <- consumeWithRead
    f <- consumeWithRead
    g <- consumeWithRead
    return [a, b, c, d, e, f, g]

main :: IO ()
main = do 
    {-putStrLn "Start Servering in 10000 port: "-}
    {-serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do-}
        {-[>maybe_msg <- recv connectionSocket <]-}
        {-putStrLn $ "TCP connection established from " ++ show remoteAddr-}
    let (buf, val) = execPipeline (fromString "sdfsdf"::B.ByteString) (parser::Pipeline B.ByteString [B.ByteString])
    print (buf, val)
