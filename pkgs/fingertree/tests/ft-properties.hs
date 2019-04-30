{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- QuickCheck properties for Data.FingerTree

module Main where

import Data.FingerTree    -- needs to be compiled with -DTESTING for use here

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (Assertion, (@?=))
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Poly

import Prelude hiding (null, reverse, foldl, foldl1, foldr, foldr1, all)
import qualified Prelude

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Data.Foldable (Foldable(foldMap, foldl, foldr), toList, all)
import Data.Functor ((<$>))
import Data.Traversable (traverse)
import Data.List (inits)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))

main :: IO ()
main = defaultMainWithOpts
    [ testProperty "foldr" prop_foldr
    , testProperty "foldl" prop_foldl
    , testProperty "(==)" prop_equals
    , testProperty "compare" prop_compare
    , testProperty "mappend" prop_mappend
    , testCase "empty" test_empty
    , testProperty "singleton" prop_singleton
    , testProperty "(<|)" prop_cons
    , testProperty "(|>)" prop_snoc
    , testProperty "(><)" prop_append
    , testProperty "fromList" prop_fromList
    , testProperty "null" prop_null
    , testProperty "viewl" prop_viewl
    , testProperty "viewr" prop_viewr
    , testCase "search" test_search
    , testProperty "search" prop_search
    , testProperty "split" prop_split
    , testProperty "takeUntil" prop_takeUntil
    , testProperty "dropUntil" prop_dropUntil
    , testProperty "reverse" prop_reverse
    , testProperty "fmap'" prop_fmap'
    -- , testProperty "fmapWithPos" prop_fmapWithPos -- (slow)
    , testProperty "traverse'" prop_traverse'
    -- , testProperty "traverseWithPos" prop_traverseWithPos -- (slow)
    ] runner_opts
  where
    runner_opts = mempty { ropt_test_options = Just test_opts }
    test_opts = mempty {
          topt_maximum_generated_tests = Just 500
        , topt_maximum_unsuitable_generated_tests = Just 500
        }

{--------------------------------------------------------------------
  The general plan is to compare each function with a list equivalent.
  Each operation should produce a valid tree representing the same
  sequence as produced by its list counterpart on corresponding inputs.
  (The list versions are often lazier, but these properties ignore
  strictness.)
--------------------------------------------------------------------}

-- utilities for partial conversions

infix 4 ~=

(~=) :: Eq a => Maybe a -> a -> Bool
(~=) = maybe (const False) (==)

-- Partial conversion of an output sequence to a list.
toList' :: (Eq a, Measured [a] a, Valid a) => Seq a -> Maybe [a]
toList' xs
  | valid xs = Just (toList xs)
  | otherwise = Nothing

toListPair' ::
    (Eq a, Measured [a] a, Valid a, Eq b, Measured [b] b, Valid b) =>
        (Seq a, Seq b) -> Maybe ([a], [b])
toListPair' (xs, ys) = (,) <$> toList' xs <*> toList' ys

-- instances

prop_foldr :: Seq A -> Bool
prop_foldr xs =
    foldr f z xs == Prelude.foldr f z (toList xs)
  where
    f = (:)
    z = []

prop_foldl :: Seq A -> Bool
prop_foldl xs =
    foldl f z xs == Prelude.foldl f z (toList xs)
  where
    f = flip (:)
    z = []

prop_equals :: Seq OrdA -> Seq OrdA -> Bool
prop_equals xs ys =
    (xs == ys) == (toList xs == toList ys)

prop_compare :: Seq OrdA -> Seq OrdA -> Bool
prop_compare xs ys =
    compare xs ys == compare (toList xs) (toList ys)

prop_mappend :: Seq A -> Seq A -> Bool
prop_mappend xs ys =
    toList' (mappend xs ys) ~= toList xs ++ toList ys

-- * Construction

test_empty :: Assertion
test_empty =
    toList' (empty :: Seq A) @?= Just []

prop_singleton :: A -> Bool
prop_singleton x =
    toList' (singleton x) ~= [x]

prop_cons :: A -> Seq A -> Bool
prop_cons x xs =
    toList' (x <| xs) ~= x : toList xs

prop_snoc :: Seq A -> A -> Bool
prop_snoc xs x =
    toList' (xs |> x) ~= toList xs ++ [x]

prop_append :: Seq A -> Seq A -> Bool
prop_append xs ys =
    toList' (xs >< ys) ~= toList xs ++ toList ys

prop_fromList :: [A] -> Bool
prop_fromList xs =
    toList' (fromList xs) ~= xs

-- * Deconstruction

prop_null :: Seq A -> Bool
prop_null xs =
    null xs == Prelude.null (toList xs)

prop_viewl :: Seq A -> Bool
prop_viewl xs =
    case viewl xs of
    EmptyL ->   Prelude.null (toList xs)
    x :< xs' -> valid xs' && toList xs == x : toList xs'

prop_viewr :: Seq A -> Bool
prop_viewr xs =
    case viewr xs of
    EmptyR ->   Prelude.null (toList xs)
    xs' :> x -> valid xs' && toList xs == toList xs' ++ [x]

prop_search :: Int -> Seq A -> Bool
prop_search n xs =
    case search p xs of
        Position _ b _ -> Just b == indexFromEnd n (toList xs)
        OnLeft         -> n >= len || null xs
        OnRight        -> n < 0
        Nowhere        -> error "impossible: the predicate is monotonic"
  where p vl vr = Prelude.length vl >= len - n && Prelude.length vr <= n

        len = length xs

        indexFromEnd :: Int -> [a] -> Maybe a
        indexFromEnd i = listToMaybe . drop i . Prelude.reverse


test_search :: Assertion
test_search = do
    lookupByIndexFromEnd xs1 1 @?= Just (A 4)
    lookupByIndexFromEnd xs2 1 @?= Just (A 4)
  where
    xs1 = Deep (map A [1..5]) (Four (A 1) (A 2) (A 3) (A 4)) Empty (One (A 5))
    xs2 = Deep (map A [1..5]) (One (A 1)) Empty (Four (A 2) (A 3) (A 4) (A 5))
    lookupByIndexFromEnd xs n =
        let len = length xs
            p vl vr = Prelude.length vl >= len - n && Prelude.length vr <= n
        in case search p xs of
               Position _ x _ -> Just x
               _              -> Nothing

prop_split :: Int -> Seq A -> Bool
prop_split n xs =
    toListPair' (split p xs) ~= Prelude.splitAt n (toList xs)
  where p ys = Prelude.length ys > n

prop_takeUntil :: Int -> Seq A -> Bool
prop_takeUntil n xs =
    toList' (takeUntil p xs) ~= Prelude.take n (toList xs)
  where p ys = Prelude.length ys > n

prop_dropUntil :: Int -> Seq A -> Bool
prop_dropUntil n xs =
    toList' (dropUntil p xs) ~= Prelude.drop n (toList xs)
  where p ys = Prelude.length ys > n

-- * Transformation

prop_reverse :: Seq A -> Bool
prop_reverse xs =
    toList' (reverse xs) ~= Prelude.reverse (toList xs)

prop_fmap' :: Seq A -> Bool
prop_fmap' xs =
    toList' (fmap' f xs) ~= map f (toList xs)
  where f = Just

prop_fmapWithPos :: Seq A -> Bool
prop_fmapWithPos xs =
    toList' (fmapWithPos f xs) ~= zipWith f (inits xs_list) xs_list
  where
    f = (,)
    xs_list = toList xs

prop_traverse' :: Seq A -> Bool
prop_traverse' xs =
    toList' (evalM (traverse' f xs)) ~= evalM (traverse f (toList xs))
  where
    f x = do
        n <- step
        return (n, x)

prop_traverseWithPos :: Seq A -> Bool
prop_traverseWithPos xs =
    toList' (evalM (traverseWithPos f xs)) ~= evalM (traverse (uncurry f) (zip (inits xs_list) xs_list))
  where
    f xs y = do
        n <- step
        return (xs, n, y)
    xs_list = toList xs

{- untested:
traverseWithPos
-}

------------------------------------------------------------------------
-- QuickCheck
------------------------------------------------------------------------

instance (Arbitrary a, Measured v a) => Arbitrary (FingerTree v a) where
    arbitrary = sized arb
      where
        arb :: (Arbitrary a, Measured v a) => Int -> Gen (FingerTree v a)
        arb 0 = return Empty
        arb 1 = Single <$> arbitrary
        arb n = deep <$> arbitrary <*> arb (n `div` 2) <*> arbitrary

    shrink (Deep _ (One a) Empty (One b)) = [Single a, Single b]
    shrink (Deep _ pr m sf) =
        [deep pr' m sf | pr' <- shrink pr] ++
        [deep pr m' sf | m' <- shrink m] ++
        [deep pr m sf' | sf' <- shrink sf]
    shrink (Single x) = map Single (shrink x)
    shrink Empty = []

instance (Arbitrary a, Measured v a) => Arbitrary (Node v a) where
    arbitrary = oneof [
        node2 <$> arbitrary <*> arbitrary,
        node3 <$> arbitrary <*> arbitrary <*> arbitrary]

    shrink (Node2 _ a b) =
        [node2 a' b | a' <- shrink a] ++
        [node2 a b' | b' <- shrink b]
    shrink (Node3 _ a b c) =
        [node2 a b, node2 a c, node2 b c] ++
        [node3 a' b c | a' <- shrink a] ++
        [node3 a b' c | b' <- shrink b] ++
        [node3 a b c' | c' <- shrink c]

instance Arbitrary a => Arbitrary (Digit a) where
    arbitrary = oneof [
        One <$> arbitrary,
        Two <$> arbitrary <*> arbitrary,
        Three <$> arbitrary <*> arbitrary <*> arbitrary,
        Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]

    shrink (One a) = map One (shrink a)
    shrink (Two a b) = [One a, One b]
    shrink (Three a b c) = [Two a b, Two a c, Two b c]
    shrink (Four a b c d) = [Three a b c, Three a b d, Three a c d, Three b c d]

------------------------------------------------------------------------
-- Valid trees
------------------------------------------------------------------------

class Valid a where
    valid :: a -> Bool

instance (Measured v a, Eq v, Valid a) => Valid (FingerTree v a) where
    valid Empty = True
    valid (Single x) = valid x
    valid (Deep s pr m sf) =
        s == measure pr `mappend` measure m `mappend` measure sf &&
        valid pr && valid m && valid sf

instance (Measured v a, Eq v, Valid a) => Valid (Node v a) where
    valid node = measure node == foldMap measure node && all valid node

instance Valid a => Valid (Digit a) where
    valid = all valid

instance Valid A where
    valid = const True

instance Valid (a,b) where
    valid = const True

instance Valid (a,b,c) where
    valid = const True

instance Valid (Maybe a) where
    valid = const True

instance Valid [a] where
    valid = const True

------------------------------------------------------------------------
-- Use list of elements as the measure
------------------------------------------------------------------------

type Seq a = FingerTree [a] a

instance Measured [A] A where
    measure x = [x]

instance Measured [OrdA] OrdA where
    measure x = [x]

instance Measured [Maybe a] (Maybe a) where
    measure x = [x]

instance Measured [(a, b)] (a, b) where
    measure x = [x]

instance Measured [(a, b, c)] (a, b, c) where
    measure x = [x]

------------------------------------------------------------------------
-- Simple counting monad
------------------------------------------------------------------------

newtype M a = M (Int -> (Int, a))

runM :: M a -> Int -> (Int, a)
runM (M m) = m

evalM :: M a -> a
evalM m = snd (runM m 0)

instance Monad M where
    return x = M $ \ n -> (n, x)
    M u >>= f = M $ \ m -> let (n, x) = u m in runM (f x) n

instance Functor M where
    fmap f (M u) = M $ \ m -> let (n, x) = u m in (n, f x)

instance Applicative M where
    pure = return
    (<*>) = ap

step :: M Int
step = M $ \ n -> (n+1, n)
