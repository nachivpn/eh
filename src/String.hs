module String where

import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Control.Monad.State

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

-- ctci 1.5
nEditsAway :: Int -> String -> String -> Bool
nEditsAway n xs ys = evalState (opt xs ys) Map.empty <= n
        where
            -- min number of edits required to transform
            opt :: String -> String -> State (Map.Map (String,String) Int) Int
            opt [] y                    = return $ length y
            opt x []                    = return $ length x
            opt xs@(x:txs) ys@(y:tys)   = do
                value <- Map.lookup (xs , ys) <$> get
                case value of
                    Just rv -> return rv
                    Nothing -> do
                        del <- opt txs ys
                        ch <- opt txs tys
                        add <- opt xs tys
                        let rv = min' (del + 1) (ch + aux x y) (add + 1)
                        map' <- Map.insert (xs , ys) rv <$> get
                        put map'
                        return rv
            aux x y 
                | x == y    = 0
                | otherwise = 1
            min' a b c = min a (min b c)

-- ctci 1.6
stringCompression :: String -> String
stringCompression "" = ""
stringCompression original@(x:xs)
    | length compressed < length original = compressed 
    | otherwise = original
    where
    compressed = stringCompression' x 1 xs
    stringCompression' :: Char -> Int -> String -> String
    stringCompression' c n "" = c : show n
    stringCompression' c n (x:xs) 
        | c /= x = (c : show n) ++ stringCompression' x 1 xs
        | otherwise = stringCompression' c (n+1) xs

isPalindrome :: String -> Bool
isPalindrome str = reverse str == str