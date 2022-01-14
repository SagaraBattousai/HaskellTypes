module Trees where

import Control.Applicative
--import qualified 
import Data.Foldable -- as F

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)

instance Functor BTree where
  fmap f Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r) 

instance Applicative BTree where
  pure x = Node x Empty Empty
  liftA2 _ Empty _ = Empty
  liftA2 _ _ Empty = Empty
  liftA2 f (Node x l1 r1) (Node y l2 r2) =
    Node (f x y) (liftA2 f l1 l2) (liftA2 f r1 r2)

instance Foldable BTree where
  -- foldMap :: Monoid m => (a -> m) -> BTree a -> m
  foldMap _ Empty = mempty
  foldMap f (Node x l r) = 
    mappend (mappend (foldMap f l) (f x) ) (foldMap f r)


add :: Ord a => a -> BTree a -> BTree a
add x Empty = Node x Empty Empty
add x (Node y l r)
  | x <= y = Node y (add x l) r
  | otherwise = Node y l (add x r)

cbalTree :: Int -> a -> BTree a
--cbalTree 1 v = pure v --newTree v
cbalTree k v
  | k == 0 = Empty
  | k == 1 = pure v
  | otherwise = Node v (cbalTree m v) (cbalTree o v)
    where
      l = k - 1
      m = div l 2
      o = l - m 

cbalTreeTest :: Int -> BTree Char
cbalTreeTest k = cbalTree k 'x'

quotDiff :: Integral a => a -> a -> (a,a)
quotDiff x y = let z = div x y
               in (z, x - z)
