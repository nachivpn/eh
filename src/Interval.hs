module Interval where

type Interval a = (a,a)

isLt :: Ord a => Interval a ->  Interval a -> Bool
isLt (_,b) (_,d) = b < d

isOverlap :: Ord a => Interval a -> Interval a -> Bool
isOverlap (_,b) (c,_) = b >= c

isort :: Ord a => [Interval a] -> [Interval a] 
isort [] = []
isort (i@(a,b):xs) = isort left ++ i : isort right
    where
        left = [ x | x <- xs,  x `isLt` i]
        right = [ x | x <- xs, not (x `isLt` i)]

interval_scheduling :: Ord a => 
    [Interval a]        -- | list of itervals
    -> [Interval a]     -- | list of intervals that are pairwise disjoint in sorted order
interval_scheduling = reverse . is [] . isort
    where
        is ss []     = ss
        is ss (x:xs) = is (x : ss) (removeOverlapping x xs)
        removeOverlapping x = dropWhile (isOverlap x)

-- | Tests
disjointIntervals :: Ord a => [Interval a] -> Bool
disjointIntervals [] = True
disjointIntervals (x:xs) = (isDisjoint x xs) && (disjointIntervals xs)
    where
        isDisjoint x xs = and $ map (not . isOverlap x) xs

prop_is_safety :: [Interval Int] -> Bool
prop_is_safety = disjointIntervals . interval_scheduling