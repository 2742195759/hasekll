module Main (main) where

import qualified Data.ByteString as B
import Data.String (fromString)
import Lib
import Buffer
import           Network.Simple.TCP
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

--- Monad Transformer Learning by practice
--
newtype MaybeT m a = { runMaybeT :: m (Maybe a) }
instance Monad m => Monad (MaybeT m) where 
    return = MaybeT . return . Just
    x >>= f = maybeT $ do -- Monad m
        maybe_v <- runMaybeT x
        case maybe_v of 
            Just v -> runMaybeT $ f v
            Nothing -> Nothing


getPassphrase2 :: MaybeT IO String
getPassphrase2 = do
    

getPassphrase :: IO (Maybe String)
getPassphrase = do  
    s <- getLine
    if 'a' `elem` s then return $ Just s
    else return Nothing

checkPassword :: IO ()
checkPassword = do 
    maybe_value <- getPassphrase
    case maybe_value of 
        Just value -> putStrLn value
        Nothing -> putStrLn "wrong."


main :: IO ()
main = do 
    {-putStrLn "Start Servering in 10000 port: "-}
    {-serve (Host "127.0.0.1") "10000" $ \(connectionSocket, remoteAddr) -> do-}
        {-[>maybe_msg <- recv connectionSocket <]-}
        {-putStrLn $ "TCP connection established from " ++ show remoteAddr-}

    
    -- simple cases for Pipeline Monad.
    {-let (buf, val) = execPipeline (fromString "sdfsdf"::B.ByteString) (parser::Pipeline B.ByteString [B.ByteString])-}
    {-print (buf, val)-}


    -- simple cases for Pipeline (IO Buffer)
    checkPassword
    



{-c :: IO (Maybe a) -}
{-test = do  -- IO-}
    {-value <- c-}
        {-do -- can't do maybe operator-}
            {-can't do -}
    {-case value of -}
        {-Nothing -> return Nothing-}
        {-Just x  -> IOfunc x-}

    {-IO-}

{-MonadTransformer-}

{-d :: IO (Maybe a)-}

