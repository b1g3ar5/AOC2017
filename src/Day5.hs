{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module Day5 where


import Prelude -- hiding (length)
import Data.Maybe
import Data.Map.Strict (Map, insert, (!), size, fromList)
import Control.Monad.ST (runST, ST(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Array.MArray
import Data.Array.ST
import Utils ( getLines )

import System.TimeIt

move1 :: (Int, Int, Map Int Int) -> Int
move1 (steps, p, ns) = if p<0 || p> (size ns - 1) then
                         steps
                       else
                         move1 (steps+1, p + ns ! p, insert p (ns!p + 1) ns)


move2slow :: (Int, Int, Map Int Int) -> Int
move2slow (steps, p, ns) 
  | p < 0 || p > (size ns - 1) = steps
  | otherwise = move2slow (steps+1, p + v, insert p newVal ns)
  where 
    v = ns!p
    newVal = if v >= 3 then v-1 else v+1

move2 :: [Int]-> Int
move2 xs = runST $ 
  do
    let bounds = (0, length xs - 1)
    mem <- newListArray bounds xs :: ST s (STUArray s Int Int)
    let 
        step ip k | inRange bounds ip = do
          v <- readArray mem ip
          let newV = if v >= 3 then v-1 else v+1
          writeArray mem ip newV
          step (ip + v) $! k + 1
        step _ k = return k
    step 0 0

day5 :: IO ()
day5 = do
  inLines <- getLines 5
  let ns :: [Int]
      ns = read <$> inLines
      mp :: Map Int Int
      mp = fromList $ zip [0..] ns

  putStrLn $ "Day5: part1: " ++ show (move1 (0, 0, mp))
  --timeIt $ putStrLn $ "Day5: part2: " ++ show (move2slow (0, 0, mp))
  putStrLn $ "Day5: part2: " ++ show (move2 ns)

  return ()


