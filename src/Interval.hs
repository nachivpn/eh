module Interval where

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