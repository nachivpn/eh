module Interval where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Test.QuickCheck

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
interval_scheduling = is [] . isort snd
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

prop_is_safety :: [Interval Int] -> Property
prop_is_safety xs = valid_intervals ==> 
    disjointIntervals . isort snd . interval_scheduling $ xs
    where
        valid_intervals = and [ a <= b | (a,b) <- xs]

prop_ip_safety :: [Interval Int] -> Bool
prop_ip_safety = and . map disjointIntervals . interval_partitioning

----------------------------------
-- Weighted interval Scheduling --
----------------------------------

type WInterval a = (Interval a, Int)

type WIResult a = (Int, [WInterval a]) 
type WIState a = Map.Map (WInterval a) (WIResult a)

weighted_scheduling :: Ord a =>
    [WInterval a]              -- | list of itervals
    -> WIResult a    -- | intervals that are pairwise disjoint & have max total weight
weighted_scheduling xs = evalState (opt . reverse . wisort snd $ xs) Map.empty
        where
            opt :: Ord a => [WInterval a] -> State (WIState a) (WIResult a)
            opt []  = return (0,[])
            opt (x@(_,w):xs) = do
                value <- Map.lookup x <$> get
                case value of
                    Just rv -> return rv
                    Nothing  -> do
                        (a, aws) <- opt xs
                        (b, bws) <- opt (removeOverlap x xs)
                        let b' = (b + w)
                        if a > b' then do
                            -- if x is NOT in the solution
                            let rv = (a, aws)
                            map' <- Map.insert x rv <$> get
                            put map'
                            return rv
                        else do
                            -- if x IS in the solution
                            let rv = (b', x:bws)
                            map' <- Map.insert x rv <$> get
                            put map'
                            return rv

-- exhaustive search to maximize total wieght of weighted interval scheduling
weighted_scheduling_ex_opt :: Ord a =>
    [WInterval a]       -- | list of itervals
    -> Int              -- | optimal total weight of pairwise disjoint intervals
weighted_scheduling_ex_opt = opt . reverse . wisort snd
        where
            opt :: Ord a => [WInterval a] -> Int
            opt []              = 0
            opt (x@(_,w):xs)    = max (opt xs) (opt (removeOverlap x xs) + w)
               
-- | Utilities

-- | Sort a list of wieghted intervals
wisort :: Ord a => (Interval a -> a) -> [WInterval a] -> [WInterval a] 
wisort f []              = []
wisort f (wi@(i,_):xs)    = wisort f left ++ wi : wisort f right
    where
        left = [ x | x@(xi,xw) <- xs,  xi `isLtf` i]
        right = [ x | x@(xi,xw) <- xs, not $ xi `isLtf` i]
        isLtf = isLt f

-- remove overlapping intervals
removeOverlap x = dropWhile (isOverlapRW x)

-- compute if intervals overlap considering reverse order
isOverlapRW (x,_) (y,_) = flip isOverlap x y


-- tests for weighted interval scheduling

prop_sound_wis :: [WInterval Int] -> Bool
prop_sound_wis xs = let (w, wis) = weighted_scheduling xs in
    w == foldr (\(_,w) acc -> acc + w) 0 wis

prop_wisw_equals_ex_opt :: [WInterval Int] -> Bool
prop_wisw_equals_ex_opt xs = let (w, wis) = weighted_scheduling xs in
    w == weighted_scheduling_ex_opt xs

disjointWIntervals :: Ord a => [WInterval a] -> Bool
disjointWIntervals = disjointIntervals . isort snd . map fst

prop_wis_safety :: [WInterval Int] -> Property
prop_wis_safety xs = valid_intervals ==> 
    let (w, wis) = weighted_scheduling xs in disjointWIntervals wis
    where
        valid_intervals = and [ a <= b | ((a,b),_) <- xs]