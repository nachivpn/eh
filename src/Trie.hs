module Trie where

import qualified Data.Map.Strict as Map
import Test.QuickCheck

-- |Trie is a tree, where branches are indexed by characters and
-- a path in the tree corresponds to one word.
-- The end of a word is marked by the Boolean in 'Node'.
data Trie a = Node Bool (Map.Map a (Trie a)) | Leaf
    deriving Show

type Dictionary = Trie Char

-- |makes a trie from a word
mkTrie :: Ord a => [a] -> Trie a
mkTrie [] = Node True Map.empty
mkTrie (a:as) = Node False (Map.insert a (mkTrie as) Map.empty)

-- |insert a word into a trie
insert :: Ord a => [a] -> Trie a -> Trie a
insert as     Leaf              = mkTrie as
insert []     (Node b trmap)    = Node True trmap
insert (a:as) (Node b trmap)    =
    case Map.lookup a trmap of
        Nothing -> Node b (Map.insert a (mkTrie as) trmap)
        Just tr -> Node b (Map.insert a (insert as tr) trmap)

-- |checks if a word exists in a trie
exists :: Ord a => [a] -> Trie a -> Bool
exists _        Leaf            = False
exists []       (Node b _)      = b
exists (a:as)   (Node b trmap)  =
    case Map.lookup a trmap of
        Nothing -> False
        Just tr -> exists as tr

-- |checks if a word matching a pattern exists in a dictionary
match :: [Char] -> Dictionary -> Bool
match _         Leaf           = False
match []        (Node b _)     = b 
match (a:as)    (Node b trmap)
    | a == '*' = any (match as) (Map.elems trmap)
    | otherwise =
        case Map.lookup a trmap of
            Nothing -> False
            Just tr -> match as tr

prop_ins :: [[Char]] -> Bool
prop_ins strs = all (flip exists ft) strs
    where
    ft = foldr insert Leaf strs