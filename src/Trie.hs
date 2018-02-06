module Trie where

import qualified Data.Map.Strict as Map

data Trie a = Node Bool (Map.Map a (Trie a)) | Leaf
    deriving Show

type Dictionary = Trie Char

mkTrie :: Ord a => [a] -> Trie a
mkTrie [] = Node True Map.empty
mkTrie (a:as) = Node False (Map.insert a (mkTrie as) Map.empty)

insert :: Ord a => [a] -> Trie a -> Trie a
insert as     Leaf              = mkTrie as
insert []     (Node b trmap)    = Node True trmap
insert (a:as) (Node b trmap)    =
    case Map.lookup a trmap of
        Nothing -> Node b (Map.insert a (mkTrie as) trmap)
        Just tr -> Node b (Map.insert a (insert as tr) trmap)

exists :: Ord a => [a] -> Trie a -> Bool
exists _        Leaf            = False
exists []       (Node b _)      = b
exists (a:as)   (Node b trmap)  =
    case Map.lookup a trmap of
        Nothing -> False
        Just tr -> exists as tr

patternMatch :: [Char] -> Dictionary -> Bool
patternMatch _     Leaf          = False
patternMatch []     (Node b _)   = b 
patternMatch (a:as) (Node b trmap)
    | a == '*' = any (patternMatch as) (Map.elems trmap)
    | otherwise =
        case Map.lookup a trmap of
            Nothing -> False
            Just tr -> patternMatch as tr