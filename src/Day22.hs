{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}


module Day22 where

import Utils ( getLines, Coord, up, dn, lt, rt )
import Data.Map.Strict (Map(..), insert)
import qualified Data.Map.Strict as M
import System.TimeIt ( timeIt )
import Data.Stream.Tape (Tape(..), focus, tapeOf, moveR, moveL)
import qualified Data.Stream as S
import qualified Data.Stream.Tape as T

{-
I first implemented this with a 2D tape (like the Turing macine in Day25)
because we don't need the random access, only access to the cell we're at...

However - all the fmaps take ages and it went up to 40mins!

So I tried the simple map

-}


data Status = Clean | Weakened | Infected | Flagged deriving (Show, Eq, Enum)

nextS :: Status -> Status
nextS Flagged = Clean
nextS x = succ x
prevS :: Status -> Status
prevS Clean = Flagged
prevS x = pred x


data Move = Up | Rt | Dn | Lt deriving (Show, Enum)


nextM :: Move -> Move
nextM Lt = Up
nextM x = succ x
prevM :: Move -> Move
prevM Up = Lt
prevM x = pred x


data State = State {m :: Map Coord Status, pos :: Coord, mv :: Move, infected :: Int} deriving (Show)


rule1 :: Status -> Status
rule1 Clean = Infected
rule1 _ = Clean


rule2 :: Status -> Status
rule2 Clean = Weakened
rule2 Weakened = Infected
rule2 Infected = Flagged
rule2 Flagged = Clean


turn :: Move -> Coord -> Coord
turn Lt = (+ lt)
turn Up = (+ up)
turn Rt = (+ rt)
turn Dn = (+ dn)


dir :: Status -> Move -> Move
dir Clean = prevM
dir Weakened = id
dir Infected = nextM
dir Flagged = go
  where
    go Up = Dn
    go Rt = Lt
    go Dn = Up
    go Lt = Rt


part2Map :: State -> State
part2Map (State mp pos d n) = State (insert pos (rule2 st) mp) (turn newD pos) newD newN
  where
    !st = M.findWithDefault Clean pos mp
    !newN = n + if st==Weakened then 1 else 0
    !newD = dir st d


mkMap :: [String] -> Map Coord Status
mkMap xss = M.fromList $ concat $ (\(r, xs) -> (\(c, x) -> ((c, r), if x=='#' then Infected else Clean)) <$> zip rng xs) <$> zip rng xss
  where
    start = length xss `div` 2
    rng = [-start, (-start+1)..start]


myIterate :: (State -> State) -> State -> Int -> State
myIterate f start 0 = start
myIterate f start n = myIterate f next (n-1)
  where
    !next = f start


day22 :: IO ()
day22 = do
  inLines <- getLines 22
  let tape1 :: Tape (Tape Bool)
      tape1 = ((=='#') <$>) <$> mkTape2 '.' inLines
      tape2 = ((\b -> if b then Infected else Clean) <$>) <$> tape1
      map2 = mkMap inLines
      initialState :: State
      initialState = State map2 (0,0) Up 0
      start = (Up, 0)
     
  putStrLn $ "Day22: part1: " ++ show (snd $ snd $ iterate part1Tape (tape1, start) !! 10000)
  putStrLn $ "Day22: part2: " ++ show (infected $ myIterate part2Map initialState 10000000)


  return ()


-- ########################### SLOW CODE because of all the fmaps, but quite neat


part1Tape :: Num b => (Tape (Tape Bool), (Move, b)) -> (Tape (Tape Bool), (Move, b))
part1Tape (t, (mv, n)) = (go newD $ update2 (not st) t, (newD, newN))
  where
    !newN = n + if st then 0 else 1
    !newD = if st then nextM mv else prevM mv
    st = focus $ focus t
    go Up = mvU
    go Lt = mvL
    go Rt = mvR
    go Dn = mvD

update2 :: a -> Tape (Tape a) -> Tape (Tape a)
update2 x (Tape l (Tape ll y rr) r)= Tape l (Tape ll x rr) r


mvR, mvD, mvL, mvU :: Tape (Tape a) -> Tape (Tape a)
mvR t = moveR <$> t
mvL t = moveL <$> t
mvU = moveL
mvD = moveR 


mkTape :: a -> [a] -> Tape a
mkTape dflt xs = T.unfold (iter llst) (const $ xs!!ln) (iter rlst) 0
  where
    n = length xs
    m = n `div` 2
    ln = if odd n then m else m-1
    rn = if odd n then m+1 else m
    llst = reverse $ take ln xs
    rlst = drop rn xs
    --iter :: [a] -> Int -> (a, Int)
    iter xs n
      | n>=length xs = (dflt, n)
      | otherwise = (xs!!n, n+1)


mkTape2 :: a -> [[a]] -> Tape (Tape a)
mkTape2 dflt xss = mkTape (tapeOf dflt) $ mkTape dflt <$> xss


instance Show a => Show (Tape a) where
  show (Tape l f r) = "\nTape [" ++ show (S.take 10 l) ++ "], " ++ show f ++ " [" ++ show (S.take 10 r) ++ "]"