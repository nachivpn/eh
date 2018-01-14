module String where

import Data.List (sort, nub)

-- O (n log n)
isUniqueChars :: String -> Bool
isUniqueChars = iuc . sort
    where
        -- O (n)
        iuc :: String -> Bool
        iuc [] = True
        iuc [x] = True
        iuc (x : y : xs) = (x /= y) && iuc (y : xs)

-- O (n^2)
reverse' :: String -> String
reverse' = foldr (\x xs -> xs ++ [x]) []

prop_reverse' :: String -> Bool
prop_reverse' xs = reverse' xs == reverse xs

-- O (n^2)
nub' :: String -> String
nub' []       = [] 
nub' (x : xs) = x : nub' (filter (\y -> x /= y) xs)

prop_nub' :: String -> Bool
prop_nub' xs = (nub xs) == (nub' xs)

-- O (n log n)
isAnagram :: String -> String -> Bool
isAnagram xs ys = (sort xs) == (sort ys)

-- O (n)
replace :: String -> String
replace = foldr (\x xs -> if x == ' ' then "%20"++ xs else x : xs) []