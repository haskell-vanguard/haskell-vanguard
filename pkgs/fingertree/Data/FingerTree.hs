{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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
-- Module      :  Data.FingerTree
-- Copyright   :  (c) Ross Paterson, Ralf Hinze 2006
-- License     :  BSD-style
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- A general sequence representation with arbitrary annotations, for
-- use as a base for implementations of various collection types, as
-- described in section 4 of
--
--  * Ralf Hinze and Ross Paterson,
--    \"Finger trees: a simple general-purpose data structure\",
--    /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--    <http://staff.city.ac.uk/~ross/papers/FingerTree.html>
--
-- For a directly usable sequence type, see @Data.Sequence@, which is
-- a specialization of this structure.
--
-- An amortized running time is given for each operation, with /n/
-- referring to the length of the sequence.  These bounds hold even in
-- a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module Data.FingerTree (
#if TESTING
    FingerTree(..), Digit(..), Node(..), deep, node2, node3,
#else
    FingerTree,
#endif
    Measured(..),
    -- * Construction
    empty, singleton,
    (<|), (|>), (><),
    fromList,
    -- * Deconstruction
    null,
    -- ** Examining the ends
    ViewL(..), viewl,
    ViewR(..), viewr,
    -- ** Search
    SearchResult(..), search,
    -- ** Splitting
    -- | These functions are special cases of 'search'.
    split, takeUntil, dropUntil,
    -- * Transformation
    reverse,
    -- ** Maps
    fmap', fmapWithPos, fmapWithContext, unsafeFmap,
    -- ** Traversals
    traverse', traverseWithPos, traverseWithContext, unsafeTraverse,
    -- * Example
    -- $example
    ) where

import Prelude hiding (null, reverse)
#if MIN_VERSION_base(4,6,0)
import GHC.Generics
#endif
#if MIN_VERSION_base(4,8,0)
import qualified Prelude (null)
#else
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Data.Monoid
import Data.Foldable (Foldable(foldMap))
#endif
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Foldable (toList)

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

-- | View of the left end of a sequence.
data ViewL s a
    = EmptyL        -- ^ empty sequence
    | a :< s a      -- ^ leftmost element and the rest of the sequence
    deriving (Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

-- | View of the right end of a sequence.
data ViewR s a
    = EmptyR        -- ^ empty sequence
    | s a :> a      -- ^ the sequence minus the rightmost element,
                    -- and the rightmost element
    deriving (Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

instance (Functor s) => Functor (ViewL s) where
    fmap _ EmptyL    = EmptyL
    fmap f (x :< xs) = f x :< fmap f xs

instance (Functor s) => Functor (ViewR s) where
    fmap _ EmptyR    = EmptyR
    fmap f (xs :> x) = fmap f xs :> f x

#if MIN_VERSION_base(4,9,0)
instance (Measured v a) => Semigroup (FingerTree v a) where
    (<>) = (><)
#endif

-- | 'empty' and '><'.
instance (Measured v a) => Monoid (FingerTree v a) where
    mempty = empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (><)
#endif

-- Explicit Digit type (Exercise 1)

data Digit a
    = One a
    | Two a a
    | Three a a a
    | Four a a a a
    deriving (Show
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

instance Foldable Digit where
    foldMap f (One a) = f a
    foldMap f (Two a b) = f a `mappend` f b
    foldMap f (Three a b c) = f a `mappend` f b `mappend` f c
    foldMap f (Four a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

-------------------
-- 4.1 Measurements
-------------------

-- | Things that can be measured.
class (Monoid v) => Measured v a | a -> v where
    measure :: a -> v

instance (Measured v a) => Measured v (Digit a) where
    measure = foldMap measure

---------------------------
-- 4.2 Caching measurements
---------------------------

data Node v a = Node2 !v a a | Node3 !v a a a
    deriving (Show
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

instance Foldable (Node v) where
    foldMap f (Node2 _ a b) = f a `mappend` f b
    foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c

node2        ::  (Measured v a) => a -> a -> Node v a
node2 a b    =   Node2 (measure a `mappend` measure b) a b

node3        ::  (Measured v a) => a -> a -> a -> Node v a
node3 a b c  =   Node3 (measure a `mappend` measure b `mappend` measure c) a b c

instance (Monoid v) => Measured v (Node v a) where
    measure (Node2 v _ _)    =  v
    measure (Node3 v _ _ _)  =  v

nodeToDigit :: Node v a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

-- | A representation of a sequence of values of type @a@, allowing
-- access to the ends in constant time, and append and split in time
-- logarithmic in the size of the smaller piece.
--
-- The collection is also parameterized by a measure type @v@, which
-- is used to specify a position in the sequence for the 'split' operation.
-- The types of the operations enforce the constraint @'Measured' v a@,
-- which also implies that the type @v@ is determined by @a@.
--
-- A variety of abstract data types can be implemented by using different
-- element types and measurements.
data FingerTree v a
    = Empty
    | Single a
    | Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)
#if TESTING
    deriving (Show
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )
#elif __GLASGOW_HASKELL__ >= 706
    deriving (Generic)
#endif

deep ::  (Measured v a) =>
     Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf =
    Deep ((measure pr `mappend` measure m) `mappend` measure sf) pr m sf

-- | /O(1)/. The cached measure of a tree.
instance (Measured v a) => Measured v (FingerTree v a) where
    measure Empty           =  mempty
    measure (Single x)      =  measure x
    measure (Deep v _ _ _)  =  v

-- | Elements from left to right.
instance Foldable (FingerTree v) where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Deep _ pr m sf) =
        foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

#if MIN_VERSION_base(4,8,0)
    null Empty = True
    null _ = False
#endif

instance (Eq a) => Eq (FingerTree v a) where
    xs == ys = toList xs == toList ys

-- | Lexicographical order from left to right.
instance (Ord a) => Ord (FingerTree v a) where
    compare xs ys = compare (toList xs) (toList ys)

#if !TESTING
instance (Show a) => Show (FingerTree v a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)
#endif

-- | Like 'fmap', but with constraints on the element types.
fmap' :: (Measured v1 a1, Measured v2 a2) =>
    (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmap' = mapTree

mapTree :: (Measured v2 a2) =>
    (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
mapTree _ Empty = Empty
mapTree f (Single x) = Single (f x)
mapTree f (Deep _ pr m sf) =
    deep (mapDigit f pr) (mapTree (mapNode f) m) (mapDigit f sf)

mapNode :: (Measured v2 a2) =>
    (a1 -> a2) -> Node v1 a1 -> Node v2 a2
mapNode f (Node2 _ a b) = node2 (f a) (f b)
mapNode f (Node3 _ a b c) = node3 (f a) (f b) (f c)

mapDigit :: (a -> b) -> Digit a -> Digit b
mapDigit f (One a) = One (f a)
mapDigit f (Two a b) = Two (f a) (f b)
mapDigit f (Three a b c) = Three (f a) (f b) (f c)
mapDigit f (Four a b c d) = Four (f a) (f b) (f c) (f d)

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix of the tree to the left of the element.
fmapWithPos :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmapWithPos f = mapWPTree f mempty

mapWPTree :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> a2) -> v1 -> FingerTree v1 a1 -> FingerTree v2 a2
mapWPTree _ _ Empty = Empty
mapWPTree f v (Single x) = Single (f v x)
mapWPTree f v (Deep _ pr m sf) =
    deep (mapWPDigit f v pr)
         (mapWPTree (mapWPNode f) vpr m)
         (mapWPDigit f vm sf)
  where
    vpr     =  v    `mappend`  measure pr
    vm      =  vpr  `mappend`  measure m

mapWPNode :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> a2) -> v1 -> Node v1 a1 -> Node v2 a2
mapWPNode f v (Node2 _ a b) = node2 (f v a) (f va b)
  where
    va      = v `mappend` measure a
mapWPNode f v (Node3 _ a b c) = node3 (f v a) (f va b) (f vab c)
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b

mapWPDigit :: (Measured v a) => (v -> a -> b) -> v -> Digit a -> Digit b
mapWPDigit f v (One a) = One (f v a)
mapWPDigit f v (Two a b) = Two (f v a) (f va b)
  where
    va      = v `mappend` measure a
mapWPDigit f v (Three a b c) = Three (f v a) (f va b) (f vab c)
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b
mapWPDigit f v (Four a b c d) = Four (f v a) (f va b) (f vab c) (f vabc d)
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c

-- | Map all elements of the tree with a function that also takes the
-- measure of the prefix to the left and of the suffix to the right of
-- the element.
--
-- @since 0.1.2.0
fmapWithContext :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> v1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmapWithContext f t = mapWCTree f mempty t mempty

mapWCTree :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> v1 -> a2) -> v1 -> FingerTree v1 a1 -> v1 -> FingerTree v2 a2
mapWCTree _ _ Empty _ = Empty
mapWCTree f vl (Single x) vr = Single (f vl x vr)
mapWCTree f vl (Deep _ pr m sf) vr =
    deep (mapWCDigit f vl pr vmsr)
         (mapWCTree (mapWCNode f) vlp m vsr)
         (mapWCDigit f vlpm sf vr)
  where
    vlp     =  vl `mappend` measure pr
    vlpm    =  vlp `mappend` vm
    vmsr    =  vm `mappend` vsr
    vsr     =  measure sf `mappend` vr
    vm      =  measure m

mapWCNode :: (Measured v1 a1, Measured v2 a2) =>
    (v1 -> a1 -> v1 -> a2) -> v1 -> Node v1 a1 -> v1 -> Node v2 a2
mapWCNode f vl (Node2 _ a b) vr = node2 (f vl a vb) (f va b vr)
  where
    va      = vl `mappend` measure a
    vb      = measure b `mappend` vr
mapWCNode f vl (Node3 _ a b c) vr = node3 (f vl a vbc) (f va b vc) (f vab c vr)
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vbc     = measure b `mappend` vc
    vc      = measure c `mappend` vr

mapWCDigit ::
    (Measured v a) => (v -> a -> v -> b) -> v -> Digit a -> v -> Digit b
mapWCDigit f vl (One a) vr = One (f vl a vr)
mapWCDigit f vl (Two a b) vr = Two (f vl a vb) (f va b vr)
  where
    va      = vl `mappend` measure a
    vb      = measure b `mappend` vr
mapWCDigit f vl (Three a b c) vr = Three (f vl a vbc) (f va b vc) (f vab c vr)
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vbc     = measure b `mappend` vc
    vc      = measure c `mappend` vr
mapWCDigit f vl (Four a b c d) vr =
    Four (f vl a vbcd) (f va b vcd) (f vab c vd) (f vabc d vr)
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c
    vbcd    = measure b `mappend` vcd
    vcd     = measure c `mappend` vd
    vd      = measure d `mappend` vr

-- | Like 'fmap', but safe only if the function preserves the measure.
unsafeFmap :: (a -> b) -> FingerTree v a -> FingerTree v b
unsafeFmap _ Empty = Empty
unsafeFmap f (Single x) = Single (f x)
unsafeFmap f (Deep v pr m sf) =
    Deep v (mapDigit f pr) (unsafeFmap (unsafeFmapNode f) m) (mapDigit f sf)

unsafeFmapNode :: (a -> b) -> Node v a -> Node v b
unsafeFmapNode f (Node2 v a b) = Node2 v (f a) (f b)
unsafeFmapNode f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

-- | Like 'traverse', but with constraints on the element types.
traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverse' = traverseTree

traverseTree :: (Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseTree _ Empty = pure Empty
traverseTree f (Single x) = Single <$> f x
traverseTree f (Deep _ pr m sf) =
    deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf

traverseNode :: (Measured v2 a2, Applicative f) =>
    (a1 -> f a2) -> Node v1 a1 -> f (Node v2 a2)
traverseNode f (Node2 _ a b) = node2 <$> f a <*> f b
traverseNode f (Node3 _ a b c) = node3 <$> f a <*> f b <*> f c

traverseDigit :: (Applicative f) => (a -> f b) -> Digit a -> f (Digit b)
traverseDigit f (One a) = One <$> f a
traverseDigit f (Two a b) = Two <$> f a <*> f b
traverseDigit f (Three a b c) = Three <$> f a <*> f b <*> f c
traverseDigit f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix of the tree to the left of the element.
traverseWithPos :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseWithPos f = traverseWPTree f mempty

traverseWPTree :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> f a2) -> v1 -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseWPTree _ _ Empty = pure Empty
traverseWPTree f v (Single x) = Single <$> f v x
traverseWPTree f v (Deep _ pr m sf) =
    deep <$> traverseWPDigit f v pr <*> traverseWPTree (traverseWPNode f) vpr m <*> traverseWPDigit f vm sf
  where
    vpr     =  v    `mappend`  measure pr
    vm      =  vpr  `mappend`  measure m

traverseWPNode :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> f a2) -> v1 -> Node v1 a1 -> f (Node v2 a2)
traverseWPNode f v (Node2 _ a b) = node2 <$> f v a <*> f va b
  where
    va      = v `mappend` measure a
traverseWPNode f v (Node3 _ a b c) = node3 <$> f v a <*> f va b <*> f vab c
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b

traverseWPDigit :: (Measured v a, Applicative f) =>
    (v -> a -> f b) -> v -> Digit a -> f (Digit b)
traverseWPDigit f v (One a) = One <$> f v a
traverseWPDigit f v (Two a b) = Two <$> f v a <*> f va b
  where
    va      = v `mappend` measure a
traverseWPDigit f v (Three a b c) = Three <$> f v a <*> f va b <*> f vab c
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b
traverseWPDigit f v (Four a b c d) = Four <$> f v a <*> f va b <*> f vab c <*> f vabc d
  where
    va      = v `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c

-- | Traverse the tree from left to right with a function that also
-- takes the measure of the prefix to the left and the measure of the
-- suffix to the right of the element.
--
-- @since 0.1.2.0
traverseWithContext :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> v1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseWithContext f t = traverseWCTree f mempty t mempty

traverseWCTree :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> v1 -> f a2) -> v1 -> FingerTree v1 a1 -> v1 -> f (FingerTree v2 a2)
traverseWCTree _ _ Empty _ = pure Empty
traverseWCTree f vl (Single x) vr = Single <$> f vl x vr
traverseWCTree f vl (Deep _ pr m sf) vr =
    deep <$> traverseWCDigit f vl pr vmsr <*> traverseWCTree (traverseWCNode f) vlp m vsr <*> traverseWCDigit f vlpm sf vr
  where
    vlp     =  vl `mappend` measure pr
    vlpm    =  vlp `mappend` vm
    vmsr    =  vm `mappend` vsr
    vsr     =  measure sf `mappend` vr
    vm      =  measure m

traverseWCNode :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
    (v1 -> a1 -> v1 -> f a2) -> v1 -> Node v1 a1 -> v1 -> f (Node v2 a2)
traverseWCNode f vl (Node2 _ a b) vr = node2 <$> f vl a vb <*> f va b vr
  where
    va      = vl `mappend` measure a
    vb      = measure a `mappend` vr
traverseWCNode f vl (Node3 _ a b c) vr =
    node3 <$> f vl a vbc <*> f va b vc <*> f vab c vr
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vc      = measure c `mappend` vr
    vbc     = measure b `mappend` vc

traverseWCDigit :: (Measured v a, Applicative f) =>
    (v -> a -> v -> f b) -> v -> Digit a -> v -> f (Digit b)
traverseWCDigit f vl (One a) vr = One <$> f vl a vr
traverseWCDigit f vl (Two a b) vr = Two <$> f vl a vb <*> f va b vr
  where
    va      = vl `mappend` measure a
    vb      = measure a `mappend` vr
traverseWCDigit f vl (Three a b c) vr =
    Three <$> f vl a vbc <*> f va b vc <*> f vab c vr
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vc      = measure c `mappend` vr
    vbc     = measure b `mappend` vc
traverseWCDigit f vl (Four a b c d) vr =
    Four <$> f vl a vbcd <*> f va b vcd <*> f vab c vd <*> f vabc d vr
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c
    vd      = measure d `mappend` vr
    vcd     = measure c `mappend` vd
    vbcd    = measure b `mappend` vcd

-- | Like 'traverse', but safe only if the function preserves the measure.
unsafeTraverse :: (Applicative f) =>
    (a -> f b) -> FingerTree v a -> f (FingerTree v b)
unsafeTraverse _ Empty = pure Empty
unsafeTraverse f (Single x) = Single <$> f x
unsafeTraverse f (Deep v pr m sf) =
    Deep v <$> traverseDigit f pr <*> unsafeTraverse (unsafeTraverseNode f) m <*> traverseDigit f sf

unsafeTraverseNode :: (Applicative f) =>
    (a -> f b) -> Node v a -> f (Node v b)
unsafeTraverseNode f (Node2 v a b) = Node2 v <$> f a <*> f b
unsafeTraverseNode f (Node3 v a b c) = Node3 v <$> f a <*> f b <*> f c

-----------------------------------------------------
-- 4.3 Construction, deconstruction and concatenation
-----------------------------------------------------

-- | /O(1)/. The empty sequence.
empty :: Measured v a => FingerTree v a
empty = Empty

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> FingerTree v a
singleton = Single

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: (Measured v a) => [a] -> FingerTree v a
fromList = foldr (<|) Empty

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a <| Empty              =  Single a
a <| Single b           =  deep (One a) Empty (One b)
a <| Deep v (Four b c d e) m sf = m `seq`
    Deep (measure a `mappend` v) (Two a b) (node3 c d e <| m) sf
a <| Deep v pr m sf     =
    Deep (measure a `mappend` v) (consDigit a pr) m sf

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
consDigit _ (Four _ _ _ _) = illegal_argument "consDigit"

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: (Measured v a) => FingerTree v a -> a -> FingerTree v a
Empty |> a              =  Single a
Single a |> b           =  deep (One a) Empty (One b)
Deep v pr m (Four a b c d) |> e = m `seq`
    Deep (v `mappend` measure e) pr (m |> node3 a b c) (Two d e)
Deep v pr m sf |> x     =
    Deep (v `mappend` measure x) pr m (snocDigit sf x)

snocDigit :: Digit a -> a -> Digit a
snocDigit (One a) b = Two a b
snocDigit (Two a b) c = Three a b c
snocDigit (Three a b c) d = Four a b c d
snocDigit (Four _ _ _ _) _ = illegal_argument "snocDigit"

-- | /O(1)/. Is this the empty sequence?
null :: FingerTree v a -> Bool
null Empty = True
null _ = False

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Measured v a) => FingerTree v a -> ViewL (FingerTree v) a
viewl Empty                     =  EmptyL
viewl (Single x)                =  x :< Empty
viewl (Deep _ (One x) m sf)     =  x :< rotL m sf
viewl (Deep _ pr m sf)          =  lheadDigit pr :< deep (ltailDigit pr) m sf

rotL :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> FingerTree v a
rotL m sf      =   case viewl m of
    EmptyL  ->  digitToTree sf
    a :< m' ->  Deep (measure m `mappend` measure sf) (nodeToDigit a) m' sf

lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (One _) = illegal_argument "ltailDigit"
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Measured v a) => FingerTree v a -> ViewR (FingerTree v) a
viewr Empty                     =  EmptyR
viewr (Single x)                =  Empty :> x
viewr (Deep _ pr m (One x))     =  rotR pr m :> x
viewr (Deep _ pr m sf)          =  deep pr m (rtailDigit sf) :> rheadDigit sf

rotR :: (Measured v a) => Digit a -> FingerTree v (Node v a) -> FingerTree v a
rotR pr m = case viewr m of
    EmptyR  ->  digitToTree pr
    m' :> a ->  Deep (measure pr `mappend` measure m) pr m' (nodeToDigit a)

rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (One _) = illegal_argument "rtailDigit"
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c

digitToTree :: (Measured v a) => Digit a -> FingerTree v a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

----------------
-- Concatenation
----------------

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
(><) =  appendTree0

appendTree0 :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
appendTree0 Empty xs =
    xs
appendTree0 xs Empty =
    xs
appendTree0 (Single x) xs =
    x <| xs
appendTree0 xs (Single x) =
    xs |> x
appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2) =
    deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits0 m1 (One a) (One b) m2 =
    appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: (Measured v a) => FingerTree v a -> a -> FingerTree v a -> FingerTree v a
appendTree1 Empty a xs =
    a <| xs
appendTree1 xs a Empty =
    xs |> a
appendTree1 (Single x) a xs =
    x <| a <| xs
appendTree1 xs a (Single x) =
    xs |> a |> x
appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) =
    deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits1 m1 (One a) b (One c) m2 =
    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: (Measured v a) => FingerTree v a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree2 Empty a b xs =
    a <| b <| xs
appendTree2 xs a b Empty =
    xs |> a |> b
appendTree2 (Single x) a b xs =
    x <| a <| b <| xs
appendTree2 xs a b (Single x) =
    xs |> a |> b |> x
appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) =
    deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits2 m1 (One a) b c (One d) m2 =
    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: (Measured v a) => FingerTree v a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree3 Empty a b c xs =
    a <| b <| c <| xs
appendTree3 xs a b c Empty =
    xs |> a |> b |> c
appendTree3 (Single x) a b c xs =
    x <| a <| b <| c <| xs
appendTree3 xs a b c (Single x) =
    xs |> a |> b |> c |> x
appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) =
    deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits3 m1 (One a) b c d (One e) m2 =
    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: (Measured v a) => FingerTree v a -> a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree4 Empty a b c d xs =
    a <| b <| c <| d <| xs
appendTree4 xs a b c d Empty =
    xs |> a |> b |> c |> d
appendTree4 (Single x) a b c d xs =
    x <| a <| b <| c <| d <| xs
appendTree4 xs a b c d (Single x) =
    xs |> a |> b |> c |> d |> x
appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) =
    deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits4 m1 (One a) b c d e (One f) m2 =
    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

----------------
-- 4.4 Splitting
----------------

-- | A result of 'search', attempting to find a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
--
-- @since 0.1.2.0
data SearchResult v a
    = Position (FingerTree v a) a (FingerTree v a)
        -- ^ A tree opened at a particular element: the prefix to the
        -- left, the element, and the suffix to the right.
    | OnLeft
        -- ^ A position to the left of the sequence, indicating that the
        -- predicate is 'True' at both ends.
    | OnRight
        -- ^ A position to the right of the sequence, indicating that the
        -- predicate is 'False' at both ends.
    | Nowhere
        -- ^ No position in the tree, returned if the predicate is 'True'
        -- at the left end and 'False' at the right end.  This will not
        -- occur if the predicate in monotonic on the tree.
    deriving (Eq, Ord, Show
#if __GLASGOW_HASKELL__ >= 706
        , Generic
#endif
        )

-- | /O(log(min(i,n-i)))/. Search a sequence for a point where a predicate
-- on splits of the sequence changes from 'False' to 'True'.
--
-- The argument @p@ is a relation between the measures of the two
-- sequences that could be appended together to form the sequence @t@.
-- If the relation is 'False' at the leftmost split and 'True' at the
-- rightmost split, i.e.
--
-- @not (p 'mempty' ('measure' t)) && p ('measure' t) 'mempty'@
--
-- then there must exist an element @x@ in the sequence such that @p@
-- is 'False' for the split immediately before @x@ and 'True' for the
-- split just after it:
--
-- <<images/search.svg>>
--
-- In this situation, @'search' p t@ returns such an element @x@ and the
-- pieces @l@ and @r@ of the sequence to its left and right respectively.
-- That is, it returns @'Position' l x r@ such that
--
-- * @l >< (x <| r) = t@
--
-- * @not (p (measure l) (measure (x <| r))@
--
-- * @p (measure (l |> x)) (measure r)@
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/ on @t@.
--
-- @since 0.1.2.0
search :: (Measured v a) =>
    (v -> v -> Bool) -> FingerTree v a -> SearchResult v a
search p t
  | p_left && p_right = OnLeft
  | not p_left && p_right = case searchTree p mempty t mempty of
        Split l x r -> Position l x r
  | not p_left && not p_right = OnRight
  | otherwise = Nowhere
  where
    p_left = p mempty vt
    p_right = p vt mempty
    vt = measure t

-- isSplit :: (Measured v a) => (v -> v -> Bool) -> v -> a -> v -> Bool
-- isSplit p vl x vr = not (p vl (v `mappend` vr)) && p (vl `mappend` v) vr
--   where v = measure x
--
-- property:
-- isSplit p vl t vr =>
--    let Split l x r = search t in
--    isSplit p (vl `mappend` measure l) x (measure r `mappend` vr)

searchTree :: (Measured v a) =>
    (v -> v -> Bool) -> v -> FingerTree v a -> v -> Split (FingerTree v a) a
searchTree _ _ Empty _ = illegal_argument "searchTree"
searchTree _ _ (Single x) _ = Split Empty x Empty
searchTree p vl (Deep _ pr m sf) vr
  | p vlp vmsr  =  let  Split l x r     =  searchDigit p vl pr vmsr
                   in   Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vlpm vsr  =  let  Split ml xs mr  =  searchTree p vlp m vsr
                        Split l x r     =  searchNode p (vlp `mappend` measure ml) xs (measure mr `mappend` vsr)
                   in   Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise   =  let  Split l x r     =  searchDigit p vlpm sf vr
                   in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where
    vlp     =  vl `mappend` measure pr
    vlpm    =  vlp `mappend` vm
    vmsr    =  vm `mappend` vsr
    vsr     =  measure sf `mappend` vr
    vm      =  measure m

searchNode :: (Measured v a) =>
    (v -> v -> Bool) -> v -> Node v a -> v -> Split (Maybe (Digit a)) a
searchNode p vl (Node2 _ a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` measure a
    vb      = measure b `mappend` vr
searchNode p vl (Node3 _ a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vc      = measure c `mappend` vr
    vbc     = measure b `mappend` vc

searchDigit :: (Measured v a) =>
    (v -> v -> Bool) -> v -> Digit a -> v -> Split (Maybe (Digit a)) a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` Split Nothing a Nothing
searchDigit p vl (Two a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` measure a
    vb      = measure b `mappend` vr
searchDigit p vl (Three a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vbc     = measure b `mappend` vc
    vc      = measure c `mappend` vr
searchDigit p vl (Four a b c d) vr
  | p va vbcd   = Split Nothing a (Just (Three b c d))
  | p vab vcd   = Split (Just (One a)) b (Just (Two c d))
  | p vabc vd   = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    va      = vl `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c
    vbcd    = measure b `mappend` vcd
    vcd     = measure c `mappend` vd
    vd      = measure d `mappend` vr

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split ::  (Measured v a) =>
      (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _ Empty  =  (Empty, Empty)
split p xs
  | p (measure xs) =  (l, x <| r)
  | otherwise   =  (xs, Empty)
  where
    Split l x r = splitTree p mempty xs

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- prefix of @t@ whose measure does not satisfy @p@.
--
-- *  @'takeUntil' p t = 'fst' ('split' p t)@
takeUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p  =  fst . split p

-- | /O(log(min(i,n-i)))/.
-- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- after removing the largest prefix whose measure does not satisfy @p@.
--
-- * @'dropUntil' p t = 'snd' ('split' p t)@
dropUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p  =  snd . split p

data Split t a = Split t a t

splitTree :: (Measured v a) =>
    (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v a) a
splitTree _ _ Empty = illegal_argument "splitTree"
splitTree _ _ (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
  | p vpr       =  let  Split l x r     =  splitDigit p i pr
                   in   Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vm        =  let  Split ml xs mr  =  splitTree p vpr m
                        Split l x r     =  splitNode p (vpr `mappend` measure ml) xs
                   in   Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise   =  let  Split l x r     =  splitDigit p vm sf
                   in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where
    vpr     =  i    `mappend`  measure pr
    vm      =  vpr  `mappend`  measure m

deepL :: (Measured v a) =>
    Maybe (Digit a) -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nothing m sf      =   rotL m sf
deepL (Just pr) m sf    =   deep pr m sf

deepR :: (Measured v a) =>
    Digit a -> FingerTree v (Node v a) -> Maybe (Digit a) -> FingerTree v a
deepR pr m Nothing      =   rotR pr m
deepR pr m (Just sf)    =   deep pr m sf

splitNode :: (Measured v a) =>
    (v -> Bool) -> v -> Node v a -> Split (Maybe (Digit a)) a
splitNode p i (Node2 _ a b)
  | p va        = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = i `mappend` measure a
splitNode p i (Node3 _ a b c)
  | p va        = Split Nothing a (Just (Two b c))
  | p vab       = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = i `mappend` measure a
    vab     = va `mappend` measure b

splitDigit :: (Measured v a) =>
    (v -> Bool) -> v -> Digit a -> Split (Maybe (Digit a)) a
splitDigit _ i (One a) = i `seq` Split Nothing a Nothing
splitDigit p i (Two a b)
  | p va        = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = i `mappend` measure a
splitDigit p i (Three a b c)
  | p va        = Split Nothing a (Just (Two b c))
  | p vab       = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = i `mappend` measure a
    vab     = va `mappend` measure b
splitDigit p i (Four a b c d)
  | p va        = Split Nothing a (Just (Three b c d))
  | p vab       = Split (Just (One a)) b (Just (Two c d))
  | p vabc      = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    va      = i `mappend` measure a
    vab     = va `mappend` measure b
    vabc    = vab `mappend` measure c

------------------
-- Transformations
------------------

-- | /O(n)/. The reverse of a sequence.
reverse :: (Measured v a) => FingerTree v a -> FingerTree v a
reverse = reverseTree id

reverseTree :: (Measured v2 a2) => (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
reverseTree _ Empty = Empty
reverseTree f (Single x) = Single (f x)
reverseTree f (Deep _ pr m sf) =
    deep (reverseDigit f sf) (reverseTree (reverseNode f) m) (reverseDigit f pr)

reverseNode :: (Measured v2 a2) => (a1 -> a2) -> Node v1 a1 -> Node v2 a2
reverseNode f (Node2 _ a b) = node2 (f b) (f a)
reverseNode f (Node3 _ a b c) = node3 (f c) (f b) (f a)

reverseDigit :: (a -> b) -> Digit a -> Digit b
reverseDigit f (One a) = One (f a)
reverseDigit f (Two a b) = Two (f b) (f a)
reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)

illegal_argument :: String -> a
illegal_argument name =
    error $ "Logic error: " ++ name ++ " called with illegal argument"

{- $example

Particular abstract data types may be implemented by defining
element types with suitable 'Measured' instances.

(from section 4.5 of the paper)
Simple sequences can be implemented using a 'Sum' monoid as a measure:

> newtype Elem a = Elem { getElem :: a }
>
> instance Measured (Sum Int) (Elem a) where
>     measure (Elem _) = Sum 1
>
> newtype Seq a = Seq (FingerTree (Sum Int) (Elem a))

Then the measure of a subsequence is simply its length.
This representation supports log-time extraction of subsequences:

> take :: Int -> Seq a -> Seq a
> take k (Seq xs) = Seq (takeUntil (> Sum k) xs)
>
> drop :: Int -> Seq a -> Seq a
> drop k (Seq xs) = Seq (dropUntil (> Sum k) xs)

The module @Data.Sequence@ is an optimized instantiation of this type.

For further examples, see "Data.IntervalMap.FingerTree" and
"Data.PriorityQueue.FingerTree".

-}
