module BinarySearchTree where

import Control.Monad.Except
import Test.QuickCheck

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

sampleTree = 
    Node 20
        (Node 8 
            (Node 4 Empty Empty) 
            (Node 12
                (Node 10 Empty Empty) 
                (Node 14 Empty Empty))) 
        (Node 22 Empty Empty)

-- |WARNING: possibly incorrect in some corner cases, needs review
commonAncestor :: Eq a => (a,a) -> Tree a -> Maybe (a, Bool)
commonAncestor (p,q) Empty = Nothing
commonAncestor (p,q) (Node a lt rt)
    | a == p = return (p, False)
    | a == q = return (q, False)
    | otherwise =
            case commonAncestor (p,q) lt of
                Nothing             -> commonAncestor (p,q) rt
                Just (x, True)      -> Just (x, True)
                Just (x, False)     ->
                    case commonAncestor (p,q) rt of
                        Just (y, True)  -> Just (y, True)
                        Just (y, False) -> Just (a, x == p && y == q)
                        Nothing -> Just (x, False)