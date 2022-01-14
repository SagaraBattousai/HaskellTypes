module Types.Trees.RBTree where

import Control.Applicative
--import qualified 
import Data.Foldable -- as F

data RBColour = Red | Black deriving (Read, Show, Eq, Ord, Enum, Bounded) 

data RBTree a = Empty | Node a (RBTree a) (RBTree a) RBColour 
  deriving (Show, Eq)

instance Functor RBTree where
  fmap f Empty = Empty
  fmap f (Node x l r c) = Node (f x) (fmap f l) (fmap f r) c 

instance Applicative RBTree where
  pure x = Node x Empty Empty Black
  liftA2 _ Empty _ = Empty
  liftA2 _ _ Empty = Empty
  liftA2 f (Node x l1 r1 c1) (Node y l2 r2 c2) -- ???
    | c1 == c2 = Node (f x y) (liftA2 f l1 l2) (liftA2 f r1 r2) c1
    | otherwise = Empty -- ?

instance Foldable RBTree where
  -- foldMap :: Monoid m => (a -> m) -> BTree a -> m
  foldMap _ Empty = mempty
  foldMap f (Node x l r _) = 
    mappend (mappend (foldMap f l) (f x)) (foldMap f r)


insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = makeBlack $ insert' x tree
  where
    insert' x Empty = Node x Empty Empty Red
    insert' x (Node y l r c)
      | x <= y = rbBalance (Node y (insert' x l) r c)
      | otherwise = rbBalance (Node y l (insert' x r) c)
    makeBlack :: RBTree a -> RBTree a
    makeBlack Empty = Empty
    makeBlack (Node n t1 t2 _) = Node n t1 t2 Black


rotateRight :: RBTree a -> RBTree a
rotateRight (Node n (Node l t1 t2 c1) t3 c2) = (Node l t1 (Node n t2 t3 c1) c2)
rotateRight t = t -- I.e. Do nothoing if first case doesn't match

rotateLeft :: RBTree a -> RBTree a
rotateLeft (Node n t1 (Node r t2 t3 c1) c2) = (Node r (Node n t1 t2 c1) t3 c2)
rotateLeft t = t -- I.e. Do nothoing if first case doesn't match

treeHeight :: RBTree a -> Int
treeHeight Empty = 0
treeHeight t = treeHeight' t 0
  where
    treeHeight' :: RBTree a -> Int -> Int
    treeHeight' Empty c = c
    treeHeight' (Node n l r _) c = maxSubHeight
      where
        leftMaxHeight = treeHeight' l (c + 1)
        rightMaxHeight = treeHeight' r (c + 1)
        maxSubHeight = max leftMaxHeight rightMaxHeight

treeBlackHeight :: RBTree a -> Int
treeBlackHeight Empty = 0
treeBlackHeight t = treeBlackHeight' t 0
  where
    treeBlackHeight' :: RBTree a -> Int -> Int
    treeBlackHeight' Empty c = c
    treeBlackHeight' (Node n l r col) c = maxSubHeight
      where
        leftMaxHeight = treeBlackHeight' l count
        rightMaxHeight = treeBlackHeight' r count
        maxSubHeight = max leftMaxHeight rightMaxHeight
        count = if col == Black then c + 1 else c

rbBalance :: RBTree a -> RBTree a
-- 1)
rbBalance Empty = Empty
-- 2) -> Just make root black after each insertion
-- 3)
rbBalance tree@(Node g (Node p1 c1 c2 Red) (Node p2 c3 c4 Red) col)
  |any isRed [c1, c2, c3, c4] = 
    (Node g (Node p1 c1 c2 Black) (Node p2 c3 c4 Black) Red)
  |otherwise = tree -- No change
{-- 
 - Since Red parent and uncle patterns is already matched can
 - assume uncle is black if not specified.
--}
-- 4a)
rbBalance (Node g (Node p t1 (Node c t2 t3 Red) Red) u _) =
  Node c (Node p t1 t2 Red) (Node g t3 u Red) Black
-- 5a)
rbBalance (Node g (Node p (Node c t1 t2 Red) t3 Red) u _) =
  Node p (Node c t1 t2 Red) (Node g t3 u Red) Black
-- 4b)
rbBalance (Node g u (Node p (Node c t1 t2 Red) t3 Red) _) =
  Node c (Node g u t1 Red) (Node p t2 t3 Red) Black
-- 5b)
rbBalance (Node g u (Node p t1 (Node c t2 t3 Red) Red) _) =
  Node p (Node g u t1 Red) (Node c t2 t3 Red) Black
-- Must be last so as to exhaust all patterns
rbBalance t = t

nodeColour :: RBTree a -> RBColour
nodeColour Empty = Black
nodeColour (Node _ _ _ c) = c

isRed :: RBTree a -> Bool
isRed Empty = False
isRed (Node _ _ _ c) = c == Red

{--
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

--}
