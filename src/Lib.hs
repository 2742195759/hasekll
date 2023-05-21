{-# LANGUAGE InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

safeAdd :: Int -> Maybe Int 
safeAdd x 
    | x > 0 = Just $ x + 1
    | otherwise = Nothing

safeDo :: Int -> Maybe Int
safeDo x = do 
    x <- safeAdd x
    safeAdd x

data Logger a = Logger a String
                    deriving (Show, Eq)


add1 :: Int -> Logger Int
add1 x = Logger (x + 1) "Add 1"


sub1 :: Int -> Logger Int
sub1 x = Logger (x - 2) "Sub 2"


someThing :: Int -> Logger Int
someThing x = let Logger value1 logger1 = add1 x 
              in let Logger value2 logger2 = sub1 value1
                 in  Logger value2 (logger1 ++ "\n" ++ logger2)

class MyClass a where 
    tolist :: a -> [a]
    tolist x = [x]

data Point = Point { x:: Int, y:: Int}

instance Functor Logger
instance Applicative Logger
instance Monad Logger where
    return x = Logger x ""

    {-(>>=) :: Logger a -> (a -> Logger b) -> Logger b-}
    (Logger va la) >>= f = let Logger vb lb = f va in 
                                Logger vb (la ++ lb)

someThing2 :: Int -> Logger Int
someThing2 x = do 
    x <-add1 x
    sub1 x

