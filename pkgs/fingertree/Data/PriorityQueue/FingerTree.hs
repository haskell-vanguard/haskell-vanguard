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
-- Min-priority queues implemented using the 'FingerTree' type,
-- following section 4.6 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- These have the same big-O complexity as skew heap implementations,
-- but are approximately an order of magnitude slower.
-- On the other hand, they are stable, so they can be used for fair
-- queueing.  They are also shallower, so that 'fmap' consumes less
-- space.
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

module Data.PriorityQueue.FingerTree (
    PQueue,
    -- * Construction
    empty,
    singleton,
    union,
    insert,
    add,
    fromList,
    -- * Deconstruction
    null,
    minView,
    minViewWithKey
    ) where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, (<|), (|>), (><), ViewL(..), Measured(..))

import Prelude hiding (null)
#if MIN_VERSION_base(4,6,0)
import GHC.Generics
#endif
#if MIN_VERSION_base(4,8,0)
import qualified Prelude (null)
#else
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
#endif
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Control.Arrow ((***))
import Data.List (unfoldr)

data Entry k v = Entry k v
#if __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif

instance Functor (Entry k) where
    fmap f (Entry k v) = Entry k (f v)

instance Foldable (Entry k) where
    foldMap f (Entry _ v) = f v

data Prio k v = NoPrio | Prio k v
#if __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif

#if MIN_VERSION_base(4,9,0)
instance Ord k => Semigroup (Prio k v) where
    (<>) = unionPrio
#endif

instance Ord k => Monoid (Prio k v) where
    mempty  = NoPrio
#if !(MIN_VERSION_base(4,11,0))
    mappend = unionPrio
#endif

unionPrio :: Ord k => Prio k v -> Prio k v -> Prio k v
x `unionPrio` NoPrio      = x
NoPrio `unionPrio` y      = y
x@(Prio kx _) `unionPrio` y@(Prio ky _)
  | kx <= ky            = x
  | otherwise           = y

instance Ord k => Measured (Prio k v) (Entry k v) where
    measure (Entry k v) = Prio k v

-- | Priority queues.
newtype PQueue k v = PQueue (FingerTree (Prio k v) (Entry k v))
#if __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif

instance Ord k => Functor (PQueue k) where
    fmap f (PQueue xs) = PQueue (FT.fmap' (fmap f) xs)

-- | In ascending order of keys.
instance Ord k => Foldable (PQueue k) where
    foldMap f q = case minView q of
        Nothing -> mempty
        Just (v, q') -> f v `mappend` foldMap f q'
#if MIN_VERSION_base(4,8,0)
    null (PQueue q) = FT.null q
#endif

#if MIN_VERSION_base(4,9,0)
instance Ord k => Semigroup (PQueue k v) where
    (<>) = union
#endif

-- | 'empty' and 'union'
instance Ord k => Monoid (PQueue k v) where
    mempty = empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = union
#endif

instance (Ord k, Eq v) => Eq (PQueue k v) where
    xs == ys = assocs xs == assocs ys

-- | Lexicographical ordering
instance (Ord k, Ord v) => Ord (PQueue k v) where
    compare xs ys = compare (assocs xs) (assocs ys)

-- | In ascending key order
instance (Ord k, Show k, Show v) => Show (PQueue k v) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (assocs xs)

-- | /O(1)/. The empty priority queue.
empty :: Ord k => PQueue k v
empty = PQueue FT.empty

-- | /O(1)/. A singleton priority queue.
singleton :: Ord k => k -> v -> PQueue k v
singleton k v = PQueue (FT.singleton (Entry k v))

-- | /O(log n)/. Add a (priority, value) pair to the front of a priority queue.
--
-- * @'insert' k v q = 'union' ('singleton' k v) q@
--
-- If @q@ contains entries with the same priority @k@, 'minView' of
-- @'insert' k v q@ will return them after this one.
insert :: Ord k => k -> v -> PQueue k v -> PQueue k v
insert k v (PQueue q) = PQueue (Entry k v <| q)

-- | /O(log n)/. Add a (priority, value) pair to the back of a priority queue.
--
-- * @'add' k v q = 'union' q ('singleton' k v)@
--
-- If @q@ contains entries with the same priority @k@, 'minView' of
-- @'add' k v q@ will return them before this one.
add :: Ord k => k -> v -> PQueue k v -> PQueue k v
add k v (PQueue q) = PQueue (q |> Entry k v)

-- | /O(log(min(n1,n2)))/. Concatenate two priority queues.
-- 'union' is associative, with identity 'empty'.
--
-- If there are entries with the same priority in both arguments, 'minView'
-- of @'union' xs ys@ will return those from @xs@ before those from @ys@.
union :: Ord k => PQueue k v -> PQueue k v -> PQueue k v
union (PQueue xs) (PQueue ys) = PQueue (xs >< ys)

-- | /O(n)/. Create a priority queue from a finite list of priorities
-- and values.
fromList :: Ord k => [(k, v)] -> PQueue k v
fromList = foldr (uncurry insert) empty

-- | /O(1)/. Is this the empty priority queue?
null :: Ord k => PQueue k v -> Bool
null (PQueue q) = FT.null q

-- | /O(1)/ for the element, /O(log(n))/ for the reduced queue.
-- Returns 'Nothing' for an empty map, or the value associated with the
-- minimal priority together with the rest of the priority queue.
--
--  * @'minView' 'empty' = 'Nothing'@
--
--  * @'minView' ('singleton' k v) = 'Just' (v, 'empty')@
--
minView :: Ord k => PQueue k v -> Maybe (v, PQueue k v)
minView q = fmap (snd *** id) (minViewWithKey q)

-- | /O(1)/ for the element, /O(log(n))/ for the reduced queue.
-- Returns 'Nothing' for an empty map, or the minimal (priority, value)
-- pair together with the rest of the priority queue.
--
--  * @'minViewWithKey' 'empty' = 'Nothing'@
--
--  * @'minViewWithKey' ('singleton' k v) = 'Just' ((k, v), 'empty')@
--
--  * If @'minViewWithKey' qi = 'Just' ((ki, vi), qi')@ and @k1 <= k2@,
--    then @'minViewWithKey' ('union' q1 q2) = 'Just' ((k1, v1), 'union' q1' q2)@
--
--  * If @'minViewWithKey' qi = 'Just' ((ki, vi), qi')@ and @k2 < k1@,
--    then @'minViewWithKey' ('union' q1 q2) = 'Just' ((k2, v2), 'union' q1 q2')@
--
minViewWithKey :: Ord k => PQueue k v -> Maybe ((k, v), PQueue k v)
minViewWithKey (PQueue q)
  | FT.null q = Nothing
  | otherwise = Just ((k, v), case FT.viewl r of
    _ :< r' -> PQueue (l >< r')
    _ -> error "can't happen")
  where
    Prio k v = measure q
    (l, r) = FT.split (below k) q

below :: Ord k => k -> Prio k v -> Bool
below _ NoPrio = False
below k (Prio k' _) = k' <= k

-- | /O(n)/. Key-value pairs in ascending key order.
assocs :: Ord k => PQueue k v -> [(k, v)]
assocs = unfoldr minViewWithKey
