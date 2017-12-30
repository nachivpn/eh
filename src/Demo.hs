module Demo where

import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List hiding (partition)
import Control.Parallel

------------------------------------------
-- Unique Haskell features
-- * Pure (potentially partial) functions
-- * Strongly and statically typed
-- * Lazy (or call by need) evaluation
------------------------------------------

------------------------------------------
-- Nice Haskell features
-- * Higher order functions
-- * Composeable programs (or functions)
-- * Easy to "verify" correctness
-- * Easy to parallelize
------------------------------------------


----------------------------------------------------------------------------------------------
--                                      101
----------------------------------------------------------------------------------------------

-- (define (partition compare l1)
--       (cond
--          ((null? l1) '())
--          ((compare (car l1)) (cons (car l1) 
-- 				(partition compare (cdr l1))))
--          (else (partition compare (cdr l1)))))

partition :: (a -> Bool) -> [a] -> [a]
partition compare []    = []
partition compare (hd:tl)
        | compare hd = hd : (partition compare tl)
        | otherwise  = partition compare tl

-- Point free style using foldr
filtr' :: (a -> Bool) -> [a] -> [a]
filtr' f = foldr (\x xs -> if (f x) then (x : xs) else xs) []

-- Concise list comprehensions!
filtr :: (a -> Bool) -> [a] -> [a]
filtr f xs = [ x | x <- xs, f x ]

-- (define (quicksort l1)
--       (cond
--          ((null? l1) '())
--          (else (let ((pivot (car l1)))
--             (append (append (quicksort (partition (lambda (x) (< x pivot)) l1))
--                        (partition (lambda (x) (= x pivot)) l1))
--                     (quicksort (partition (lambda (x) (> x pivot)) l1)))))))

quicksort :: Ord a => [a] -> [a]
quicksort []            = []
quicksort xs@(pivot:_)  = quicksort (filtr (< pivot) xs) ++
                        filtr (== pivot) xs ++
                        quicksort (filtr (> pivot) xs)

----------------------------------------------------------------------------------------------
--                              Quickcheck
----------------------------------------------------------------------------------------------


-- Is using partition the "same as" using filtr?
-- Are the two functions equal? When are functions equal?
-- f == g <-> forall x. f x == g x
-- But, *** forall x *** !
-- x may belong to an infinite domain!

-- we could try out a few random values of x
-- Enter *drumroll* QuickCheck
-- generates random values using the type !

prop_filter :: [Int] -> Fun Int Bool -> Bool
prop_filter xs (Fun _ f) = (partition f xs) == (filtr f xs)

-- Is quicksort correct?
-- Looks ok, but can ve verify that?
-- But, what do we mean by "correct"?

isOrdered :: Ord a => [a] -> Bool
isOrdered xs = and $ zipWith (<=) xs (tail xs)

-- Thanks to lazyness, we get a concise (and yet performant) specification
-- isOrdered is a "canonical" Haskell function

-- can we compose our correctness specification and the algorithm?
prop_qsort :: [Int] -> Bool
prop_qsort = isOrdered . quicksort


-- A generic sorting algorithm specification
prop_sort :: 
        ([Int] -> [Int]) -- Sorting algorithm
        -> [Int] -- input List
        -> Bool -- Is result sorted when fed to sorting algorithm?
prop_sort sort_alg = isOrdered . sort_alg


----------------------------------------------------------------------------------------------
--                              ADTs
----------------------------------------------------------------------------------------------

-- Explicit data type (+structure) declarations
-- using Alegbraic Data types (ADTs)

data List a = Nil | Cons a (List a)

data Tree a = Empty | Node a (Tree a) (Tree a)

----------------------------------------------------------------------------------------------
--                              Par
----------------------------------------------------------------------------------------------

psort [] = []
psort xs@(pivot:_) = par middle $ par right $
        left ++
        middle ++ 
        right
    where
        left = quicksort (filtr (< pivot) xs) 
        right = psort (filtr (> pivot) xs)
        middle = filtr (== pivot) xs
-- Pure function calls can be executed in parallel 
-- Since data all data is immutable in functional languages,
-- writing parallel code boils down to identifying 
-- "fruitful" parallelism in computation

-- We just wrote a parallal version of quicksort, but parallelism is tricky business
-- Is it correct? Let's ask QuickCheck!

prop_psort :: [Int] -> Bool
prop_psort = prop_sort psort
