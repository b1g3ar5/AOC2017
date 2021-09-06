{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeOperators, FlexibleInstances  #-}

module TotalMap (  TMap(..)
                      , fromPartial
                      , (!)
                      , tabulate
                      , trim
                      , intersectionPartialWith
                      , codomain
                      , mapWithKey
                      , keys
                      , assocs
                      , _map
                      , setDefault
                      , insert
                      , module M) where

--import Data.Monoid (Monoid(..),(<>))
--import Control.Applicative (Applicative(..),liftA2,(<$>))
import Data.Maybe (fromMaybe)

import Data.Map as M hiding ((!), mapWithKey, assocs, keys, insert)
import qualified Data.Map as M ((!), mapWithKey, assocs, keys, insert)
import Data.Set (Set)
import qualified Data.Set as S

import Control.Comonad
import Control.Applicative

{-
There is a Conal Eliot paper about this and a hackage library
This is a map with a default element so lookups are total and not maybe
With this we get functor, applicative, comonad and comonadapply instances.

-}

-- | Total map
data TMap i e = TMap e (Map i e) deriving (Eq)


instance (Show k, Show v) => Show (TMap k v) where
    show (TMap k v) = "TMap Default: " ++ show k ++ ", Elements: " ++ show v


keys :: TMap i e -> [i]
keys (TMap d m) = M.keys m

_map :: TMap k a -> Map k a
_map (TMap _ m) = m


-- Not sure this is a good idea!!
setDefault :: a -> TMap k a -> TMap k a
setDefault x (TMap _ m) = TMap x m

-- When the default is mapped we need the key to be the focus of the Sheet
-- This sounds nonsense!!!
mapWithKey :: (Monoid k, Eq a) => (k -> a -> b) -> TMap k a -> TMap k b
mapWithKey f ~(TMap x m) = TMap (f keyOfDefault x) $ M.mapWithKey f m
  where
    keyOfDefault = go mempty $ toList m
    go acc [] = acc
    go acc ((ix, y):iys) = if y == x then ix else go acc iys


assocs :: TMap k a -> [(k, a)]
assocs (TMap _ m) = M.assocs m

-- | Create a total map from a default value and a partial map.
fromPartial :: a -> Map k a -> TMap k a
fromPartial = TMap


-- | Sample a total map. Semantic function.
(!) :: Ord k => TMap k v -> k -> v
TMap dflt m ! k = fromMaybe dflt (M.lookup k m)


-- | Construct a total map, given a default value, a set of keys, and a
-- function to sample over that set. You might want to 'trim' the result.
tabulate :: Eq k => v -> Set k -> (k -> v) -> TMap k v
tabulate dflt ks f = TMap dflt (f <$> idMap ks)


-- | Optimize a 'TMap', weeding out any explicit default values.
-- A semantic no-op, i.e., @(!) . trim == (!)@.
trim :: (Ord k, Eq v) => TMap k v -> TMap k v
trim (TMap dflt m) = TMap dflt (M.filter (/= dflt) m)

{-
-- Variation that weeds out values equal to the default. Requires Eq.
tabulate' :: (Ord k, Eq v) => v -> Set k -> (k -> v) -> TMap k v
tabulate' = (fmap.fmap.fmap) trim tabulate
-}

-- | Intersect a total map with a partial one using an element combinator.
intersectionPartialWith ::
   (Ord k) =>
   (a -> b -> c) -> TMap k a -> M.Map k b -> M.Map k c
intersectionPartialWith f (TMap ad am) bm =
   M.intersectionWith f am bm
   `M.union`
   fmap (f ad) bm

-- | Witness the finiteness of the support concretely by giving its image.
codomain :: Ord v => TMap k v -> Set v
codomain (TMap dflt m) = S.fromList (dflt : M.elems m)

{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

-- These instances follow the principle that semantic functions (here (!))
-- must be type class morphism (TCM) for all inhabited type classes.

instance (Ord k, Semigroup v) => Semigroup (TMap k v) where
  (<>) = liftA2 (<>)


instance (Ord k, Monoid v) => Monoid (TMap k v) where
  mempty  = pure mempty


instance Functor (TMap k) where
  fmap f (TMap d m) = TMap (f d) (fmap f m)


instance Ord k => Applicative (TMap k) where
  pure v = TMap v mempty
  fs@(TMap df mf) <*> xs@(TMap dx mx) =
    tabulate (df dx)                                -- The new default
             (M.keysSet mf `mappend` M.keysSet mx)  -- The union of the keys
             ((!) fs <*> (!) xs)  -- A function from the key to the new entry


-- | Alternative implementation of (<*>) using complex Map operations. Might be
-- more efficient. Can be used for testing against the canonical implementation
-- above.
_app :: Ord k => TMap k (a -> b) -> TMap k a -> TMap k b
_app (TMap fd fm) (TMap ad am) =
   TMap (fd ad) $
      fmap ($ad) (M.difference fm am) <>
      fmap (fd$) (M.difference am fm) <>
      M.intersectionWith ($) fm am

-- Note: I'd like to 'trim' the tabulate result in <*>, but doing so would
-- require the Eq constraint on values, which breaks Applicative.


instance Ord k => Monad (TMap k) where
  return  = pure
  m >>= f = joinT (f <$> m)


joinT :: Ord k => TMap k (TMap k v) -> TMap k v
joinT (TMap (TMap dd dm) mtm) =
  TMap dd (M.mapWithKey (flip (!)) mtm `M.union` dm)


instance Comonad (TMap k) where
  extract (TMap x _) = x
  duplicate t@(TMap _ m) = TMap t $ (`TMap` m) <$> m


{-
instance R.RComonad (TMap k) where
  type RComonadCtxt (TMap k) a = Eq a
  extract (TMap x _) = x
  duplicate t@(TMap _ m) = TMap t $ (`TMap` m) <$> m
-}


instance (Ord k) => ComonadApply (TMap k) where
  s1 <@> s2 = s1 <*> s2


{-
instance (Ord k) => R.RComonadApply (TMap k) where
  type RComonadApplyCtxt (TMap k) a = Eq a
  s1 <@> s2 = s1 <*> s2
-}


idMap :: Eq k => Set k -> Map k k
idMap = fromAscList . fmap (\ k -> (k,k)) . S.toAscList


insert :: Ord k => k -> a -> TMap k a -> TMap k a
insert i x (TMap d m) = TMap d (M.insert i x m)