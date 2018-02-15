module Class where

data M
data KM

data Measure m = Measure Float
    deriving Show

class Unit a where
    toM    :: Measure a -> Measure M
    fromM  :: Measure M -> Measure a

instance Unit M where
    toM     = id
    fromM   = id

instance Unit KM where
    toM (Measure x)     = Measure (x * 1000)
    fromM (Measure x)   = Measure (x / 1000)

add :: Measure M -> Measure M -> Measure M
add (Measure x) (Measure y) = Measure (x + y)

add' :: (Unit a, Unit b, Unit c) => Measure a -> Measure b -> Measure c
add' mx my = fromM $ add (toM my) (toM my)