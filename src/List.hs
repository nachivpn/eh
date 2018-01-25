module List where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Ord (Ordering(..))
import Data.List (minimumBy, maximumBy)

-- Subset sum
-- O(n * T) time & space
-- where n is the number of elements and T is the target
subset_sum :: (Ord a , Num a) => [a] -> a -> Bool
subset_sum xs n = evalState (opt (zip [1..] xs) n) Map.empty
    where
        -- whether a subset containing sum n exists
        opt :: (Ord a , Num a) => [(Int,a)] -> a -> State (Map.Map (Int,a) Bool) Bool
        opt _  0 = return True
        opt [] n = return False
        opt ((i,x):xs) n = do
            value <- Map.lookup (i,n) <$> get 
            case value of
                Just rv -> return rv
                Nothing -> do
                    rv <- liftM2 (||) (opt xs (n-x)) (opt xs n)
                    Map.insert (i,n) rv <$> get >>= put
                    return rv

-- Subsequence sum
-- O(n * T) time & space
-- where n is the number of elements and T is the target
subseq_sum :: (Eq a, Ord a , Num a) => [a] -> a -> Bool
subseq_sum seq t = evalState (opt t (zip [1..] seq) t) Map.empty
    where
        -- whether a subsequence containing sum n or sum t exists
        opt :: (Ord a , Num a) => a -> [(Int,a)] -> a -> State (Map.Map (Int,a) Bool) Bool
        opt t _ 0  = return True
        opt t [] n = return False
        opt t ((i,x):xs) n = do
            value <- Map.lookup (i,n) <$> get
            case value of
                Just rv -> return rv
                Nothing -> do
                    rv <- liftM2 (||) (opt t xs (n-x)) (opt t xs t)
                    Map.insert (i,n) rv <$> get >>= put
                    return rv

-- ctci 8.4
-- O (n * 2^n) time & space
-- Powerset of a given set 
power_set :: [a] -> [[a]]
power_set [] = [[]]
power_set (x:xs) = let pxs = power_set xs 
    in [ x : p | p <- pxs] ++ pxs

-- length of shortest subsequence with a given target
shortestsubseq_sum :: (Eq a, Ord a , Num a) => [a] -> a -> Int
shortestsubseq_sum seq target = opt seq target
    where
        -- length of the shortest subsequence with given target
        opt _       0       = 0
        opt []      t       = maxBound      -- pseudo infinity
        opt (x:xs)  t       = min (opt xs target) (opt xs (t-x) + 1)

-- given a list of numbers, find the k'th smallest element
-- i.e., given the rank, find the element
-- O(n) time & space
select :: (Ord a, Num a) => [a] -> Int -> Maybe a
select mxs = sel mxs
    where
        sel [] k        = Nothing
        sel xs@(x:_) k
            | r > k     = sel [ y | y <- xs,y < x] k 
            | r < k     = sel [ y | y <- xs, y > x] k
            | otherwise = Just x
            where
                r = rank mxs x

-- given a list of numbers, find the rank of an element
-- O (n) time & O(1) space
rank :: Ord a => [a] -> a -> Int
rank [] k = 1
rank (x:xs) k 
    | x < k     = 1 + rank xs k
    | otherwise = rank xs k