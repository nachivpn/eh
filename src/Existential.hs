{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Existential where

-- An existential data type, for any given A, there exists a T
data T = forall a. MkT a
-- i.e, MkT :: forall a. (a -> T)

extractT :: forall b. T -> b 

-- This is not possible! Why? :)
-- foo (MkT x) = x
-- Because, if this were possible, then for a given proof of T, one could prove everything?!

-- But, this is possible for a "sound" reason
extractT (MkT x) = undefined
-- ^ undefined (or the bottom constructor) is the only constructor which can construct a value of both types a and b, 
-- where (x :: forall a. a) and (foo :: forall b. T -> b)

-- Hence, T is useless!


-- A More useful existential type
data T' = forall a. Show a => MkT' a
-- Now T' can hold a hetrogenuous show value

-- Notice how a non-bottom constructor for this is still not possible!
extractT' :: forall a. Show a => T' -> a
extractT' = undefined

-- However, we can show!
instance Show T' where
    -- show :: T' -> String
    show (MkT' x) = show x

-- A heterogenuous list
type HList = [T']

hl :: HList
hl = [MkT' 5, MkT' (), MkT' "65", MkT' [1]]

-- this is the only useful computation which can be done HList i.e, "showing"
usefulHlComp = mapM_ print hl


-- Some other interesting consequences :
-- * NOTE: the stuff below requires more thought, may not be fully correct !!! *

i2s :: Int -> String
i2s n = show n

-- It is *impossible* to give valid (except undefined or non-terminating) arguments to f0
f0 :: c -> (forall a b. a -> b) -> c
f0 c f = c
-- Why?
-- Because we cannot construct a value of type: forall a b. (a -> b)
-- proof: TODO

-- non-terminating function of type (forall a b. a -> b)
ntfab :: forall a b. a -> b
ntfab x = ntfab x

-- undefined function of type (forall a b. a -> b)
unfab :: forall a b. a -> b
unfab x = undefined

-- Similarly, consider:
-- forall a. a -> forall b. b
-- constructing a value of this type is impossible!
-- because this means that (∃ a -> forall b. b), and how can one ever construct such a type?
-- Because how can a value of a given type produce a value of ALL possible types?! 
-- No such value exists of type (forall b. b) in the first place!
-- Or in other words, how can a given proof of a, prove everything in the universe?

-- Here's a more formal proof:
-- 1. forall a. a -> forall b. b    given
-- 2. | ∃ a                 |       assume
-- 3. | a0                  |       ∃-elimination on 2, fresh a0
-- 4. | a0 -> forall b. b   |       by forall elimination on 1, using a0 again
-- 5. | forall b. b         |       -> elimination on 3,4
-- 6. | ¬ a0                |       by forall elimination on 1, using ¬a0
-- 7. | ⊥                   |       ¬ elimination (or -> elimination) 3,6
-- 8. ¬ ∃ a                         ¬ introduction (or -> introduction) 2-7

-- Just because ¬ ∃ a, does not mean (forall a. a -> forall b. b) does not hold
-- It is a perfectly valid formula as it only says: IF any given a, then any given b holds.
-- Hence, (forall a. a -> forall b. b) is a valid type, but one cannot construct a value for it!

-- But see it in action for yourself
f1 :: forall a. a -> forall b. b
f1 = undefined


-- On the otherhand, these work perfectly well!

-- A simple map
map' :: forall a b . (a -> b) -> [a] -> [b]
map' f [] = []
map' f (y : ys) = f y : map' f ys

-- A simple apply
apply :: forall a b . (a -> b) -> a -> b
apply f x = f x

-- Now, Why?
-- Maybe understanding why this works for methods of type
-- requires understand how polymorphism works :)
-- Read chapter from TPL, Pierce!
-- How about: http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf
-- But can we reason about this?

-- (forall a) in Haskell is simply {a : Set} in Agda ? In this case, the proofs above can be written as propositional proofs..


-- UNDERSTANDING PARAMETRIC POLYMORPHISM WHEN USING RANK-N TYPES


-- This polymorphic function works. Does it say something about how Haskell's polymorpshim works in general?
atoInt :: forall a. a -> Int
atoInt _ = 42

-- This also a polymorphic function, but on b
f2 :: b -> (forall a. a -> b) -> b
f2 b f = f b

-- This function isn't polymorphic AT ALL!
-- the type of notPolyF is monomorphic (i.e, a concrete type)
notPolyF ::  (forall a. a -> a) -> Int
notPolyF f = f 3
-- The idea is that when we do `notPolyF id` the evaluation demands an argument, 
-- which is a value (or function if you like) of type (forall a. a -> a)
-- for this reason,

i2i :: Int -> Int
i2i x = x

-- This does not type check!
-- wrong = notPolyF i2i
-- this is because notPolyF expects a polymorphic function as an argument