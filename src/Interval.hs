module Interval where

import Control.Monad.State
import qualified Data.Map.Strict as Map

type Interval a = (a,a)

-- O (1)
isLt :: Ord a => (Interval a -> a) -> Interval a ->  Interval a -> Bool
isLt f x y = f x < f y

-- O (1)
isOverlap :: Ord a => Interval a -> Interval a -> Bool
isOverlap (_,b) (c,_) = b >= c

-- O (n)
isOverlapL :: Ord a => Interval a -> [Interval a] -> Bool
isOverlapL x = or . map (isOverlap x)

-- O (n ^ 2)
isort :: Ord a => (Interval a -> a) -> [Interval a] -> [Interval a] 
isort f []              = []
isort f (i@(a,b):xs)    = isort f left ++ i : isort f right
    where
        left = [ x | x <- xs,  x `isLtf` i]
        right = [ x | x <- xs, not $ x `isLtf` i]
        isLtf = isLt f

-- O (n ^ 2)
interval_scheduling :: Ord a => 
    [Interval a]        -- | list of itervals
    -> [Interval a]     -- | list of intervals that are pairwise disjoint in sorted order
interval_scheduling = reverse . is [] . isort snd
    where
        -- O (n ^ 2)
        is ss []     = ss
        is ss (x:xs) = is (x : ss) (removeOverlap x xs)
        -- O (n)
        removeOverlap x = dropWhile (isOverlap x)

-- O (n ^ 3)
interval_partitioning :: (Eq a, Ord a) => 
    [Interval a]        -- | list of intervals
    -> [[Interval a]]   -- | list of (list of intervals that are pairwise disjoint)
interval_partitioning = ip [] . isort fst 
        where
            -- O (n ^ 3)
            ip ss [] = ss
            ip ss (x:xs) = ip (add x ss) xs
            -- O (n ^ 2)
            add x [] = [[x]]
            add x (s : ss)
                | x `isOverlapL` s  = s : add x ss
                | otherwise         = (x : s) : ss

-- | Tests
disjointIntervals :: Ord a => [Interval a] -> Bool
disjointIntervals [] = True
disjointIntervals (x:xs) = (isDisjoint x xs) && (disjointIntervals xs)
    where
        isDisjoint x xs = and $ map (not . isOverlap x) xs

prop_is_safety :: [Interval Int] -> Bool
prop_is_safety = disjointIntervals . interval_scheduling

prop_ip_safety :: [Interval Int] -> Bool
prop_ip_safety = and . map disjointIntervals . interval_partitioning

-- weighted interval
type WInterval a = (Interval a, Int)

wisort :: Ord a => (Interval a -> a) -> [WInterval a] -> [WInterval a] 
wisort f []              = []
wisort f (wi@(i,_):xs)    = wisort f left ++ wi : wisort f right
    where
        left = [ x | x@(xi,xw) <- xs,  xi `isLtf` i]
        right = [ x | x@(xi,xw) <- xs, not $ xi `isLtf` i]
        isLtf = isLt f

-- exhaustive search solution to maximizing weighted interval scheduling
weighted_scheduling_ex :: Ord a =>
    [WInterval a]       -- | list of itervals
    -> Int              -- | optimal total weight of pairwise disjoint intervals
weighted_scheduling_ex = opt . reverse . wisort snd
        where
            opt :: Ord a => [WInterval a] -> Int
            opt []              = 0
            opt (x@(_,w):xs)    = max (opt xs) (opt (removeOverlap x xs) + w)
            -- remove overlapping intervals
            removeOverlap x = dropWhile (isOverlapRW x)
            -- compute if intervals overlap considering reverse order
            isOverlapRW (x,_) (y,_) = flip isOverlap x y

-- Dynamic programming solution to maximizing weighted interval scheduling
weighted_scheduling_dp :: Ord a =>
    [WInterval a]       -- | list of itervals
    -> Int              -- | optimal total weight of pairwise disjoint intervals
weighted_scheduling_dp xs = evalState (opt (reverse $ wisort snd $ xs)) Map.empty 
                where
                    opt :: Ord a => [WInterval a] -> State (Map.Map (WInterval a) Int) Int
                    opt []  = return 0
                    opt (x@(_,w):xs) = do
                        value <- Map.lookup x <$> get
                        case value of
                            Just x -> return x
                            Nothing  -> do
                                a <- opt xs
                                b <- opt (removeOverlap x xs)
                                return $ max a (b + w)
                    -- remove overlapping intervals
                    removeOverlap x = dropWhile (isOverlapRW x)
                    -- compute if intervals overlap considering reverse order
                    isOverlapRW (x,_) (y,_) = flip isOverlap x y

-- takes forever to complete, why?
-- either the size of tests need to be reduced
-- or this says something about the style of dynamic programming used
prop_ex_equals_dp :: [WInterval Int] -> Bool
prop_ex_equals_dp xs = weighted_scheduling_ex xs == weighted_scheduling_dp xs

weighted_scheduling :: Ord a =>
    [WInterval a]       -- | list of itervals
    -> [WInterval a]    -- | intervals that are pairwise disjoint & have max total weight
weighted_scheduling = undefined