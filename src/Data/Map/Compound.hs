{-# LANGUAGE TypeFamilies, LambdaCase, Rank2Types, ScopedTypeVariables #-}
module Data.Map.Compound where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Proxy

class Key k where
  type Map k :: * -> *
  empty :: proxy k -> Map k a
  lookup :: k -> Map k a -> Maybe a
  insertWith :: (a -> a -> a) -> k -> a -> Map k a -> Map k a
  foldMapWithKey :: Monoid b => (k -> a -> b) -> Map k a -> b
  unionWithKey :: (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
  intersectionWithKey :: (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c

singleton :: forall k a. Key k => k -> a -> Map k a
singleton k a = insertWith const k a (empty (Proxy :: Proxy k))

instance Key () where
  type Map () = Maybe
  empty _ = Nothing
  lookup _ = id
  insertWith _ _ a Nothing = Just a
  insertWith f _ a (Just b) = Just $! f a b
  foldMapWithKey f = foldMap (f ())
  unionWithKey f (Just a) (Just b) = Just $! f () a b
  unionWithKey _ (Just a) Nothing = Just a
  unionWithKey _ Nothing (Just a) = Just a
  unionWithKey _ _ _ = Nothing
  intersectionWithKey f (Just a) (Just b) = Just $! f () a b
  intersectionWithKey _ _ _ = Nothing

newtype PairMap i j a = PairMap { unPairMap :: Map i (Map j a) }

instance (Key i, Key j) => Key (i, j) where
  type Map (i, j) = PairMap i j
  empty _ = PairMap (empty (Proxy :: Proxy i))
  lookup (i, j) (PairMap m) = lookup i m >>= lookup j
  insertWith f (i, j) a (PairMap m) = PairMap
    $ insertWith (unionWithKey (\(_ :: j) -> f)) i (singleton j a) m
  foldMapWithKey f = foldMapWithKey (\i -> foldMapWithKey (f . (,) i)) . unPairMap
  unionWithKey f (PairMap a) (PairMap b) = PairMap $ unionWithKey (\i -> unionWithKey (f . (,) i)) a b
  intersectionWithKey f (PairMap a) (PairMap b) = PairMap $ intersectionWithKey (\i -> intersectionWithKey (f . (,) i)) a b

newtype Ordered k = Ordered { getOrdered :: k }

instance Ord k => Key (Ordered k) where
  type Map (Ordered k) = Map.Map k
  empty _ = Map.empty
  lookup = Map.lookup . getOrdered
  insertWith f = Map.insertWith f . getOrdered
  foldMapWithKey f = Map.foldMapWithKey (f . Ordered)
  unionWithKey f = Map.unionWithKey (f . Ordered)
  intersectionWithKey f = Map.intersectionWithKey (f . Ordered)
