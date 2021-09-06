{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Ring (
  Ring(..)
  , mkRing
  , distribute
  , collect
  , unfoldN
  , iterateN
  , enumerateN
  , moveL
  , moveR
  , Ring.zipWith
  , focus
  , rotate
  , normalise
  , (Ring.!!)
  , fromList
  , toList
) where

import Prelude hiding (zipWith, iterate, take, head, tail, init, last, (++), foldr, replicate, cycle, (<$>))
import qualified Prelude as P ((++))

import Control.Comonad hiding ((<$>))
import Control.Arrow hiding ((<$>))
import Data.Distributive hiding ((<$>))
import qualified Data.List as L (intercalate)
import Data.Vector hiding (and, iterateN, imap, zipWith, length, (<$>), fromList, toList)
import qualified Data.Vector as V (zipWith, imap, length, fromList, toList)
import qualified Data.Foldable as F (foldr)



-- the cycle is added to the index to determine what to get back
-- it's the focus
data Ring a = Ring {
    tSize :: Int
  , focusIx :: Int
  , view :: Vector a
                     } deriving (Functor)


-- | Returns the information at the focus
-- the !! is relative to the focus so its RingOld!!0
focus :: Ring a -> a
focus t = t Ring.!! 0


-- | Sets the focus index
setFocus :: Int -> Ring a -> Ring a
setFocus fx (Ring gSize _ xs) = Ring gSize fx xs


(!!) :: Ring a -> Int -> a
(Ring gSize f xs) !! n = xs ! ((n + f) `mod` gSize)


-- Makes a RingOld from a list - takes only the first gSize elements
mkRing :: a -> [a] -> Ring a
mkRing x [] = Ring 1 0 $ V.fromList [x]
mkRing x xs = Ring n 0 $ V.fromList xs
  where
    n = length xs


fromList :: [a] -> Ring a
fromList xs = Ring (length xs) 0 $ V.fromList xs


toList ::  Ring a -> [a]
toList r = V.toList $ view $ normalise r


zipWith :: (a -> b -> c) -> Ring a -> Ring b -> Ring c
zipWith f xs ys = Ring (min (tSize xs) (tSize ys)) 0 $ V.zipWith f (view nxs) (view nys)
  where
    nxs = normalise xs
    nys = normalise ys


unfoldN :: Int -> (c -> Maybe (a,c)) -> c -> Ring a
unfoldN n next x = Ring n 0 $ unfoldrN n next x


-- | Produce a @RingOld@ consisting of the infinite iteration of two functions to a starting focus value,
--   ala iterate for lists or @Stream@s.
--iterate :: (a -> a) -> a -> RingOld a
--iterate next = unfold $ \x -> Just (x, next x)
iterateN :: Int -> (a -> a) -> a -> Ring a
iterateN n next = unfoldN n $ \x -> Just (x, next x)

-- | Given an enumerable type, produce the @RingOld@ where the left side is the sequence of predecessors,
--   and the right side is the sequence of successors.
--enumerate :: (Enum a) => a -> RingOld a
--enumerate = iterate succ
enumerateN :: (Enum a) => Int -> a -> Ring a
enumerateN n x = iterateN n succ x


rotate :: Int -> Ring a -> Ring a
rotate n t@(Ring gSize f xs) = Ring gSize ((f+n) `mod` gSize) xs


-- This sets the focus to 0 and spins the xs round
normalise :: Ring a -> Ring a
normalise (Ring gSize fx xs) = Ring gSize 0 $ V.imap (\i _ -> xs!((i+fx) `mod` gSize)) xs


imap :: (Int -> a -> b) -> Ring a -> Ring b
imap f (Ring gSize fc xs) = Ring gSize fc $ V.imap f xs


instance Foldable Ring where
  foldr f z (Ring _ fc vv) = foldr f z vv


-- | RingOlds form a comonad, where extract gives the focus element and duplicate gives a /diagonalized/
--   @RingOld (RingOld a)@ such that @extract . extract . moveL . duplicate == extract . moveL@ and likewise
--   for @moveR@.
instance Comonad Ring where
   extract = focus
   duplicate t = iterateN n moveR t
    where
      n = tSize t


-- | Applying one RingOld to another moves them together. This is like the @Applicative@ instances for
--   @ZipList@ and @Stream@.
instance ComonadApply Ring where
   f <@> x = zipWith ($) f x


-- | The functions @moveR@ and @moveL@ move the focus on the RingOld right and left, respectively.
moveL, moveR :: Ring a -> Ring a
moveL = rotate (-1)
moveR = rotate 1


instance Show a => Show (Ring a) where
  show t = "The length is: " P.++ show n P.++ ". First 3 are: " P.++ L.intercalate ", " (V.toList $ take 3 $ fmap show $ view $ normalise t)
    where
      n = V.length $ view t


instance Eq a => Eq (Ring a) where
  t1 == t2 = and (V.zipWith (==) xs ys)
    where
      (Ring _ _ xs) = normalise t1
      (Ring _ _ ys) = normalise t2
