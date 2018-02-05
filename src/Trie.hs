module Trie where

import qualified Data.Map.Strict as Map

data Trie a = Node (Map.Map a (Trie a)) | Leaf
    deriving Show

type Dictionary = Trie Char

mkTrie :: Ord a => [a] -> Trie a
mkTrie [] = Leaf
mkTrie (a:as) = Node (Map.insert a (mkTrie as) Map.empty)

insert :: Ord a => [a] -> Trie a -> Trie a
insert []     tr            = tr
insert as     Leaf          = mkTrie as
insert (a:as) (Node trmap)  = 
    case Map.lookup a trmap of
        Nothing -> Node (Map.insert a (mkTrie as) trmap)
        Just tr -> let tr' = insert as tr in
            Node (Map.insert a tr' trmap)

exists :: Ord a => [a] -> Trie a -> Bool
exists []     tr            = True
exists as     Leaf          = False
exists (a:as) (Node trmap)  = 
    case Map.lookup a trmap of
        Nothing -> False
        Just tr -> exists as tr

patternMatch :: [Char] -> Dictionary -> Bool
patternMatch []     Leaf          = True
patternMatch []     _             = False
patternMatch as     Leaf          = False
patternMatch (a:as) (Node trmap)
    | a == '*' = any (patternMatch as) (Map.elems trmap)
    | otherwise =
        case Map.lookup a trmap of
            Nothing -> False
            Just tr -> patternMatch as tr