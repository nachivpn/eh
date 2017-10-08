{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Lambda where

-----------------------------------
--- Church Booleans
-----------------------------------

type CBool = forall a. a -> a -> a

true :: CBool
true = \ a b -> a

false :: CBool
false = \ a b -> b

and :: CBool -> CBool -> CBool
and = \ a b -> a b false

or :: CBool -> CBool -> CBool
or = \ a b -> a true b

not :: CBool -> CBool
not = \ a -> a false true

------------------------------------
--- Church Pair
------------------------------------

type CPair a b = forall c. (a -> b -> c) -> c

pair :: a -> b -> CPair a b
pair = \ f s b -> b f s

fst :: CPair a b -> a
fst = \ p -> p (\a b -> a)
-- can't use true because true :: a -> a -> a, but we want true :: a -> b -> a 

snd :: CPair a b -> b
snd = \p -> p (\a b -> b)
-- can't use false for same reason as above

-- for using true / false for pair manipulation, 
-- we would need a definition like this:

type CBool' a b c = a -> b -> c

true' :: CBool' a b a
true' = \a b -> a

false' :: CBool' a b b
false' = \a b -> b

fst' :: CPair a b -> a
fst' = \p -> p true'

snd' :: CPair a b -> b
snd' = \p -> p false'

-- not very pretty, indeed!

------------------------------------
--- Church Natural numbers
------------------------------------

type CNat = forall a. (a -> a) -> a -> a

zero :: CNat
zero = \ s z -> z

one :: CNat
one = \ s z -> s z

two :: CNat
two = \ s z -> s (s z)

succ :: CNat -> CNat
succ = \ n -> (\s z ->  s (n s z))

succ' :: CNat -> CNat
succ' = \ n -> (\s z -> n s (s z))

plus :: CNat -> CNat -> CNat
plus = \ m n -> (\s z -> m s (n s z))

times :: CNat -> CNat -> CNat
times = \m n -> (\s z -> m (n s) z)

-- why doesn't this type check?
-- times' :: CNat -> CNat -> CNat
-- times' = \ m n -> m (plus n) zero

times'' :: CNat -> CNat -> CNat
times'' = \ m n -> m . n

-- TODO: power :: CNat -> CNat -> CNat

isZero :: CNat -> CBool
isZero = \ n -> n (\_ -> false) true

-- prd :: CNat -> CNat
-- prd = \ m -> (Lambda.fst m ss zz)

-- It gets difficult when we try to encode more complex functions such as pred
-- Why? Are the types too strict? Or does the encoding rely on "untypedness"?