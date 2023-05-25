{-# LANGUAGE FlexibleInstances #-}
module Buffer
    ( 
      BufferClass, 
      PipelineT,
      execPipeline,
      resetBuffer,
      consumeLine,
      appendBuffer, 
      peekBuffer, 
      buf_fromString, 
      buf_contain_nl
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.String (fromString)
import Data.Char (ord)
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Class

class BufferClass a where 
    buf_fromString :: String -> a
    buf_breakNL :: a -> (a, a)
    buf_tail :: a -> a
    buf_append :: a -> a -> a
    buf_contain_nl :: a -> Bool
    buf_strip :: a -> a

instance BufferClass B.ByteString where 
    buf_fromString = fromString
    buf_breakNL = B.break is_nl where 
                is_nl c = c == (fromIntegral . ord) '\n'
    buf_contain_nl = B.elem nl where
                nl = (fromIntegral . ord) '\n'
    buf_tail = B.tail 
    buf_append = B.append
    buf_strip = C8.strip

instance BufferClass String where 
    buf_fromString = id
    buf_breakNL = break is_nl where 
                is_nl c = c == '\n'
    buf_contain_nl x = '\n' `elem` x
    buf_tail = Prelude.tail
    buf_append = (++)
    buf_strip = undefined


type Buffer = String
newtype PipelineT b m a = PipelineT { runPipelineT :: b -> m (b, a) }
instance Functor (PipelineT b m)
instance Applicative (PipelineT b m)
instance (Monad m) => Monad (PipelineT b m) where
    return x = PipelineT (\m -> return (m, x))
    (>>=) mf func = PipelineT $ \buf -> do
                        (lastb, lasta) <- runPipelineT mf buf
                        runPipelineT (func lasta) lastb
    
instance (BufferClass b) => MonadTrans (PipelineT b) where 
    lift m = PipelineT $ \t -> do 
            a <- m 
            return (t, a)

execPipeline :: (BufferClass b) => PipelineT b m a -> m (b, a)
execPipeline pipe = runPipelineT pipe $ buf_fromString ""
                    
resetBuffer :: (Monad m) => b -> PipelineT b m ()
resetBuffer x = PipelineT fn where 
                fn _ = return (x, ())

consumeLine :: (BufferClass b, Monad m) => PipelineT b m b
consumeLine = PipelineT func where
                func buf = return (y, buf_strip x) where 
                   (x, y) = buf_breakNL buf

appendBuffer :: (BufferClass b, Monad m) => b -> PipelineT b m ()
appendBuffer x = PipelineT fn where 
                fn buffer = return (buf_append buffer x, ())

peekBuffer :: (Monad m) => PipelineT b m b
peekBuffer = PipelineT $ \buf -> 
                return (buf, buf)
