module BinarySearchTree where

import Control.Monad.Except
import Test.QuickCheck
import System.Random

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- | BST Invariant
bstinv :: Ord a => Tree a -> Bool
bstinv Empty = True
bstinv (Node a Empty Empty) = True
bstinv (Node a (Node l llt lrt) Empty) =
    a >= l && bstinv llt && bstinv lrt
bstinv (Node a Empty (Node r rlt rrt)) = 
    a < r && bstinv rlt &&  bstinv rrt
bstinv (Node a (Node l llt lrt) (Node r rlt rrt)) = 
    a >= l && a < r && 
    bstinv llt && bstinv lrt && 
    bstinv rlt &&  bstinv rrt

-- | Insert a node
insert :: (Ord a) => a -> Tree a -> Tree a
insert v Empty = Node v Empty Empty 
insert v (Node a lt rt) 
    | v <= a = Node a (insert v lt) rt
    | otherwise = Node a lt (insert v rt)

-- | Delete a node
delete :: (Ord a) => a -> Tree a -> Tree a
delete v Empty = Empty
delete v t@(Node a lt rt)
    | v == a = deleteRoot t
    | v < a = Node a (delete v lt) rt
    | otherwise = Node a lt (delete v rt)
                
deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node a Empty Empty) = Empty
deleteRoot (Node a lt Empty) = lt
deleteRoot (Node a Empty rt) = rt
deleteRoot (Node a lt rt) = let maxLt = treeMax lt
                                lt' = delete maxLt lt
                            in Node a lt' rt
    where
        treeMax :: (Ord a) => Tree a -> a
        treeMax Empty = error "cannot find max of empty tree!"
        treeMax (Node a _ Empty) = a
        treeMax (Node a _ rt) = treeMax rt

-- Utilities 

size :: Tree a -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

-- Arbitary instances

-- [TODO] - add some description of this arbitrary instance
instance (Ord a, Num a, Random a, Bounded a, Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = genTree 100 5
        where
            genTree _ 0 = return Empty
            genTree v x = do
                l <- choose (minBound, v)
                r <- choose (v, maxBound)
                lt <- genTree l (x-1)
                rt <- genTree r (x-1)
                frequency [(8, return $ Node v lt rt), (2, return Empty)]

-- [TODO] - generate arbitrary BST for non-bounded data types

-- Properties

prop_insdel :: Ord a => a -> Tree a -> Bool
prop_insdel a t = any bstinv [t, t', t'']
    where
        t'' = delete a t
        t' = insert a t
 
-- [TODO] - Can any of the operations be parallelized? Why or why not?
