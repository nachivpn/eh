module List where

import qualified Data.Map.Strict as Map
import Control.Monad.State

-- Subset sum
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
-- Powerset of a given set 
power_set :: [a] -> [[a]]
power_set [] = [[]]
power_set (x:xs) = let pxs = power_set xs 
    in [ x : p | p <- pxs] ++ pxs