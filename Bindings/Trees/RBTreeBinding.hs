{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Types.Bindings.Trees.RBTree where

import Foreign

import qualified Types.Trees.RBTree as RBT

foreign export capi foo :: Int -> IO Int
foreign export capi g :: Int -> Int

foo :: Int -> IO Int
foo n = return (length (f n))

g :: Int -> Int
g = sum . f


f :: Int -> [Int]
f 0 = []
f n = n :(f (n-1))

