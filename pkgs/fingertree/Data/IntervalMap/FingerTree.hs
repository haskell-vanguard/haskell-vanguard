{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PriorityQueue.FingerTree
-- Copyright   :  (c) Ross Paterson 2008
-- License     :  BSD-style
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- Interval maps implemented using the 'FingerTree' type, following
-- section 4.8 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- An amortized running time is given for each operation, with /n/
-- referring to the size of the priority queue.  These bounds hold even
-- in a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module Data.IntervalMap.FingerTree (
    -- * Intervals
    Interval(..), low, high, point,
    -- * Interval maps
    IntervalMap, empty, singleton, insert, union,
    -- * Searching
    search, intersections, dominators,
    -- * Extraction
    bounds, leastView, splitAfter
    ) where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), (<|), (><))

import Prelude hiding (null)
#if MIN_VERSION_base(4,6,0)
import GHC.Generics
#endif
#if MIN_VERSION_base(4,8,0)
import qualified Prelude (null)
#else
import Control.Applicative ((<$>))
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
import Data.Traversable (Traversable(traverse))
#endif
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Foldable (toList)

----------------------------------
-- 4.8 Application: interval trees
----------------------------------

-- | A closed interval.  The lower bound should be less than or equal
-- to the upper bound.
data Interval v = Interval v v -- ^ Lower and upper bounds of the interval.
    deriving (Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

-- | Lower bound of the interval
low :: Interval v -> v
low (Interval lo _) = lo

-- | Upper bound of the interval
high :: Interval v -> v
high (Interval _ hi) = hi

-- | An interval in which the lower and upper bounds are equal.
point :: v -> Interval v
point v = Interval v v

data Node v a = Node (Interval v) a
    deriving (Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

instance Functor (Node v) where
    fmap f (Node i x) = Node i (f x)

instance Foldable (Node v) where
    foldMap f (Node _ x) = f x

instance Traversable (Node v) where
    traverse f (Node i x) = Node i <$> f x

-- rightmost interval (including largest lower bound) and largest upper bound.
data IntInterval v = NoInterval | IntInterval (Interval v) v
#if __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif

#if MIN_VERSION_base(4,9,0)
instance Ord v => Semigroup (IntInterval v) where
    (<>) = intervalUnion
#endif

instance Ord v => Monoid (IntInterval v) where
    mempty = NoInterval
#if !(MIN_VERSION_base(4,11,0))
    mappend = intervalUnion
#endif

intervalUnion :: Ord v => IntInterval v -> IntInterval v -> IntInterval v
NoInterval `intervalUnion` i  = i
i `intervalUnion` NoInterval  = i
IntInterval _ hi1 `intervalUnion` IntInterval int2 hi2 =
    IntInterval int2 (max hi1 hi2)

instance (Ord v) => Measured (IntInterval v) (Node v a) where
    measure (Node i _) = IntInterval i (high i)

-- | Map of closed intervals, possibly with duplicates.
newtype IntervalMap v a =
    IntervalMap (FingerTree (IntInterval v) (Node v a))
#if __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif
-- ordered lexicographically by interval

instance Functor (IntervalMap v) where
    fmap f (IntervalMap t) = IntervalMap (FT.unsafeFmap (fmap f) t)

-- | Values in lexicographical order of intervals.
instance Foldable (IntervalMap v) where
    foldMap f (IntervalMap t) = foldMap (foldMap f) t
#if MIN_VERSION_base(4,8,0)
    null (IntervalMap t) = FT.null t
#endif

-- | Traverse the intervals in lexicographical order.
instance Traversable (IntervalMap v) where
    traverse f (IntervalMap t) =
        IntervalMap <$> FT.unsafeTraverse (traverse f) t

instance (Eq v, Eq a) => Eq (IntervalMap v a) where
    IntervalMap xs == IntervalMap ys = toList xs == toList ys

-- | Lexicographical ordering
instance (Ord v, Ord a) => Ord (IntervalMap v a) where
    compare (IntervalMap xs) (IntervalMap ys) = compare (toList xs) (toList ys)

instance (Show v, Show a) => Show (IntervalMap v a) where
    showsPrec p (IntervalMap ns)
      | FT.null ns = showString "empty"
      | otherwise =
        showParen (p > 0) (showIntervals (toList ns))
      where
        showIntervals [] = showString "empty"
        showIntervals (Node i x:ixs) =
            showString "insert " . showsPrec 11 i .
                showChar ' ' . showsPrec 11 x .
                showString " $ " . showIntervals ixs

#if MIN_VERSION_base(4,9,0)
-- | 'union'.
instance (Ord v) => Semigroup (IntervalMap v a) where
    (<>) = union
#endif

-- | 'empty' and 'union'.
instance (Ord v) => Monoid (IntervalMap v a) where
    mempty = empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = union
#endif

-- | /O(1)/.  The empty interval map.
empty :: (Ord v) => IntervalMap v a
empty = IntervalMap FT.empty

-- | /O(1)/.  Interval map with a single entry.
singleton :: (Ord v) => Interval v -> a -> IntervalMap v a
singleton i x = IntervalMap (FT.singleton (Node i x))

-- | /O(log n)/.  Insert an interval and associated value into a map.
-- The map may contain duplicate intervals; the new entry will be inserted
-- before any existing entries for the same interval.
insert :: (Ord v) => Interval v -> a -> IntervalMap v a -> IntervalMap v a
insert (Interval lo hi) _ m | lo > hi = m
insert i x (IntervalMap t) = IntervalMap (l >< Node i x <| r)
  where
    (l, r) = FT.split larger t
    larger (IntInterval k _) = k >= i
    larger NoInterval = error "larger NoInterval"

-- | /O(m log (n/\//m))/.  Merge two interval maps.
-- The map may contain duplicate intervals; entries with equal intervals
-- are kept in the original order.
union  ::  (Ord v) => IntervalMap v a -> IntervalMap v a -> IntervalMap v a
union (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys)
  where
    merge1 as bs = case FT.viewl as of
        EmptyL                  -> bs
        a@(Node i _) :< as'     -> l >< a <| merge2 as' r
          where
            (l, r) = FT.split larger bs
            larger (IntInterval k _) = k >= i
            larger NoInterval = error "larger NoInterval"
    merge2 as bs = case FT.viewl bs of
        EmptyL                  -> as
        b@(Node i _) :< bs'     -> l >< b <| merge1 r bs'
          where
            (l, r) = FT.split larger as
            larger (IntInterval k _) = k > i
            larger NoInterval = error "larger NoInterval"

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
intersections :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
intersections i = inRange (low i) (high i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- in lexicographical order.
dominators :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
dominators i = inRange (high i) (low i)

-- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- in lexicographical order.
search :: (Ord v) => v -> IntervalMap v a -> [(Interval v, a)]
search p = inRange p p

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
inRange :: (Ord v) => v -> v -> IntervalMap v a -> [(Interval v, a)]
inRange lo hi (IntervalMap t) = matches (FT.takeUntil (greater hi) t)
  where
    matches xs  =  case FT.viewl (FT.dropUntil (atleast lo) xs) of
        EmptyL    ->  []
        Node i x :< xs'  ->  (i, x) : matches xs'

-- | /O(1)/.  @'bounds' m@ returns @'Nothing'@ if @m@ is empty, and
-- otherwise @'Just' i@, where @i@ is the smallest interval containing
-- all the intervals in the map.
--
-- @since 0.1.3.0
bounds :: (Ord v) => IntervalMap v a -> Maybe (Interval v)
bounds (IntervalMap t) = case measure t of
    NoInterval -> Nothing
    IntInterval _ hi -> case FT.viewl t of
        EmptyL -> Nothing
        Node (Interval lo _) _ FT.:< _ -> Just (Interval lo hi)

-- | /O(1)/.  @'leastView' m@ returns @'Nothing'@ if @m@ is empty, and
-- otherwise @'Just' ((i, x), m')@, where @i@ is the least interval,
-- @x@ is the associated value, and @m'@ is the rest of the map.
--
-- @since 0.1.3.0
leastView :: Ord v =>
    IntervalMap v a -> Maybe ((Interval v, a), IntervalMap v a)
leastView (IntervalMap t) = case FT.viewl t of
    EmptyL -> Nothing
    Node i a FT.:< t' -> Just ((i, a), IntervalMap t')

-- | /O(log(min(i,n-i)))/.  @'splitAfter' k m@ returns a pair of submaps,
-- one consisting of intervals whose lower bound is less than or equal
-- to @k@, and the other of those whose lower bound is greater.
--
-- @since 0.1.3.0
splitAfter :: Ord v =>
    v -> IntervalMap v a -> (IntervalMap v a, IntervalMap v a)
splitAfter k (IntervalMap t) = (IntervalMap before, IntervalMap after)
  where
    (before, after) = FT.split (greater k) t

atleast :: (Ord v) => v -> IntInterval v -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ NoInterval = error "atleast NoInterval"

greater :: (Ord v) => v -> IntInterval v -> Bool
greater k (IntInterval i _) = low i > k
greater _ NoInterval = error "greater NoInterval"

{-
-- Examples

mkMap :: (Ord v) => [(v, v, a)] -> IntervalMap v a
mkMap = foldr ins empty
  where
    ins (lo, hi, n) = insert (Interval lo hi) n

composers :: IntervalMap Int String
composers = mkMap [
    (1685, 1750, "Bach"),
    (1685, 1759, "Handel"),
    (1732, 1809, "Haydn"),
    (1756, 1791, "Mozart"),
    (1770, 1827, "Beethoven"),
    (1782, 1840, "Paganini"),
    (1797, 1828, "Schubert"),
    (1803, 1869, "Berlioz"),
    (1810, 1849, "Chopin"),
    (1833, 1897, "Brahms"),
    (1838, 1875, "Bizet")]

mathematicians :: IntervalMap Int String
mathematicians = mkMap [
    (1642, 1727, "Newton"),
    (1646, 1716, "Leibniz"),
    (1707, 1783, "Euler"),
    (1736, 1813, "Lagrange"),
    (1777, 1855, "Gauss"),
    (1811, 1831, "Galois")]
-}
