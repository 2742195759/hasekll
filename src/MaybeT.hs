module MaybeT
    ( 
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity (Identity, runIdentity)

instance Functor m => Functor (MaybeT m)
instance Applicative m => Applicative (MaybeT m) 
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Monad m => Monad (MaybeT m) where 
    return = MaybeT . return . Just
    x >>= f = MaybeT $ do -- Monad m
        maybe_v <- runMaybeT x
        case maybe_v of 
            Just v ->  runMaybeT $ f v
            Nothing -> return Nothing


instance MonadTrans MaybeT where 
    lift m = MaybeT $ fmap Just m 


{-instance (Monad m) => MonadIO (MaybeT m) where-}
    {-liftIO m = lift-}

{-String  ->  m String-}

  {-|             |-}
  {-|             |-}

{-String  ->  (MaybeT m) String-}


getPassphrase2 :: MaybeT IO String
getPassphrase2 = do 
    s <- lift getLine
    if 'a' `elem` s then return s
    else MaybeT $ return Nothing
    

checkPassword2 :: MaybeT IO ()
checkPassword2 = do 
    passwd <- getPassphrase2
    lift $ putStrLn passwd


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

test:: IO ()
test = do 
    -- simple cases for MonadTran
    m <- runMaybeT checkPassword2
    case m of
        Nothing -> print ("Invalid.")
        _ -> print ("Success.")
