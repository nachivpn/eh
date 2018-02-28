{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Family where
import qualified Data.IntMap as IntMap
import qualified Data.Char as Char

----------------
-- Data family
----------------

-- modular strict list data type
data family XList a

data instance XList Char = XCons !Char !(XList Char) | XNil

data instance XList () = XListUnit !Int

-- samples
cList :: XList Char
cList = XCons 'a' XNil

tList :: XList ()
tList = XListUnit 5

-------------------
-- GADTs version
-------------------

data XListG a where
    XConsG :: !Char -> !(XListG Char) -> XListG Char
    XNilG :: XListG Char
    XListUnitG :: !Int -> XListG ()
-- obviously less modular as it is not as extensible as data families

----------------------------------
-- Generalized tries, an example
----------------------------------

-- data family in a class (which is one option to use them)
class GMapKey k where
    -- an instantiable  data type
    data GMap k :: * -> * -- must have one or more of the class parameters
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
    data GMap Int v = GMapInt (IntMap.IntMap v)

    empty = GMapInt IntMap.empty
    
    lookup n (GMapInt m) = IntMap.lookup n m

    insert k v (GMapInt m) = GMapInt (IntMap.insert k v m)

instance GMapKey Char where
    data GMap Char v = GMapChar (GMap Int v)

    empty = GMapChar empty
    
    lookup n (GMapChar m) = Family.lookup (Char.ord n) m

    insert k v (GMapChar m) = GMapChar (insert (Char.ord k) v m)
