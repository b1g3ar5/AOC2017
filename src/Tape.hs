{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


module Tape where

import Prelude hiding (iterate)
import Control.Comonad
import Data.Distributive
import Control.Applicative
import qualified Data.List as L


data Tape a = Tape { lt :: [a], cursor :: a, rt :: [a]} deriving (Show)


mkTape :: [a] -> Tape a
mkTape xs
  -- If odd n = 2*m + 1, so m each end and one in the middle
  | odd n = Tape (reverse $ take m xs) (xs!!m) (drop (m+1) xs)
  -- If even n = 2*m, so m each end and one in the middle
  | otherwise = Tape (reverse $ take (m-1) xs) (xs!!(m-1)) (drop m xs)
  where
    n = length xs
    m = n `div` 2 


instance Semigroup a => Semigroup (Tape a) where
  (Tape l1 x1 r1) <> (Tape l2 x2 r2) = Tape (zipWith (<>) l1 l2) (x1<>x2)  (zipWith (<>) r1 r2)


instance Monoid a => Monoid (Tape a) where
  mempty = Tape [] mempty []


instance Functor Tape where
  fmap f (Tape l x r) = Tape (fmap f l) (f x) (fmap f r)


instance Applicative Tape where
   (Tape ls c rs) <*> (Tape ls' c' rs') = Tape (ls <*> ls') (c c') (rs <*> rs')
   pure  = Tape <$> pure <*> id <*> pure


extractTape :: Tape a -> a
extractTape (Tape _ x _) = x


duplicateTape :: Monoid a => Tape a -> Tape (Tape a)
duplicateTape = iterate moveL moveR


{-

instance Comonad Tape where
   extract (Tape _ x _) = x
   duplicate = iterate moveL moveR


instance ComonadApply Tape where
   (Tape ls c rs) <@> (Tape ls' c' rs') =
      Tape (ls <*> ls') (c c') (rs <*> rs')
-}

{-
instance Distributive Tape where
  distribute = unfold (fmap (focus . moveL) &&& fmap moveL)
                (fmap focus)
                (fmap (focus . moveR) &&& fmap moveR)
-}

-- We need the Monoid constraint to make it infinite
-- otherwise we don't know what to put on the end.
moveR, moveL :: Monoid a => Tape a -> Tape a
moveR (Tape lt f []) = Tape (f:lt) mempty []
moveR (Tape lt f (r:rs)) = Tape (f:lt) r rs
moveL (Tape [] f rt) = Tape [] mempty (f:rt)
moveL (Tape (l:ls) f rt) = Tape ls l (f:rt)


iterate :: (a -> a) -- ^ leftwards iteration function
        -> (a -> a) -- ^ rightwards iteration function
        -> a        -- ^ focus value
        -> Tape a
iterate prev next = unfold (dup . prev) id (dup . next)
   where dup a = (a,a)


unfold :: (c -> (a,c)) -- ^ leftwards unfolding function
       -> (c -> a)     -- ^ function giving the focus value from the seed
       -> (c -> (a,c)) -- ^ rightwards unfolding function
       -> c            -- ^ seed value
       -> Tape a
unfold prev center next = Tape <$> listUnfold prev <*> center <*> listUnfold next


listUnfold :: (c -> (a,c)) -> c -> [a]
listUnfold f c = x : listUnfold f d
  where
    (x,d) = f c


update :: a -> Tape a -> Tape a
update x (Tape l f r) = Tape l x r


update2 :: a -> Tape (Tape a) -> Tape (Tape a)
update2 x (Tape l (Tape ll y rr) r)= Tape l (Tape ll x rr) r


mvR, mvD, mvL, mvU :: Monoid a => Tape (Tape a) -> Tape (Tape a)
mvR t = moveR <$> t
mvL t = moveL <$> t
mvU = moveL
mvD = moveR 


data Move = Up | Dn | Lt | Rt deriving (Show)


turnRt, turnLt ::  Move -> Move
turnRt Rt = Dn
turnRt Dn = Lt
turnRt Lt = Up
turnRt Up = Rt
turnLt Rt = Up
turnLt Dn = Rt
turnLt Lt = Dn
turnLt Up = Lt


mkTape2 :: [[a]] -> Tape (Tape a)
mkTape2 xss = mkTape $ mkTape <$> xss


focus :: Tape (Tape a) -> a
focus (Tape _ (Tape _ x _)  _) = x


