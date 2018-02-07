module BinarySearchTree where

import Control.Monad.Except
import Test.QuickCheck
import System.Random
import Data.List (sort , nub)

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- | BST Invariant
bstinv :: (Bounded a, Ord a) => Tree a -> Bool
bstinv tr =
    bi tr (minBound, maxBound)
    where
        bi :: Ord a => Tree a -> (a, a) -> Bool
        bi Empty _                      = True
        bi (Node a lt rt) (minb,maxb)   =
            a > minb && a <= maxb &&
            bi lt (minb,a) && bi rt (a,maxb)

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

height :: Tree a -> Int
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node _ lt rt) = 
    abs (height lt - height rt) <= 1 
    && isBalanced lt && isBalanced rt

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

prop_insdel :: (Bounded a, Ord a) => a -> Tree a -> Bool
prop_insdel a t = any bstinv [t, t', t'']
    where
        t'' = delete a t
        t' = insert a t

prop_bstinv :: (Bounded a, Ord a) => Tree a -> Bool
prop_bstinv = bstinv
-- [TODO] - Can any of the operations be parallelized? Why or why not?

-- |Input list must be sorted and nub'd for 
-- constructing a bst of minimum height
min_bst :: Ord a => [a] -> Tree a
min_bst [] = Empty
min_bst as = case splt as of
    Nothing -> Empty
    Just (ls,a,rs) -> Node a (min_bst ls) (min_bst rs)
    where
        splt :: [a] -> Maybe ([a],a,[a])
        splt [] = Nothing
        splt as = let mid = length as `div` 2
                      (lh,rh) = splitAt mid as
                    in Just (lh, head rh, tail rh)

prop_min_bst :: [Int] -> Bool
prop_min_bst as = all ($ mbst) [bstinv, isBalanced]
    where
        mbst = min_bst (sort . nub $ as)