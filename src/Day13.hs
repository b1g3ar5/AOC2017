
module Day13 where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List ( findIndex )
import Control.Monad
import Utils

import Debug.Trace

type Layer = (Int, Int)
type Firewall = Map Int Int

parseLine :: String -> Layer
parseLine s = (read $ head ps, read $ ps!!1)
  where
    ps = splitOn ": " s


type Inc = Int

-- The state is my position and the position of all the scanners
type State = (Int, Map Int (Inc, Int))

type Severity = Int
type Delay = Int

trip :: Delay -> Firewall -> Severity
trip d fw = go 0 (-1, initialScanners ) 
  where
    initialScanners = iterate (M.mapWithKey update) (M.map (const (1, 0)) fw) !! d
    mx = maximum (M.keys fw)
    go :: Int -> State -> Int
    go score (me, scanners)
      | me == mx = newScore
      | otherwise = go newScore (me+1, newScanners)
        where
          newScore
            | M.notMember (me+1) scanners = score
            | snd (scanners!(me+1)) == 0 = score + (me+1) * (fw!(me+1))
            | otherwise = score
          newScanners = M.mapWithKey update scanners

    update :: Int -> (Inc, Int) -> (Int, Int)
    update k (i, x)
      | (x+i) < 0 = (-i, 1)
      | (x+i) == (fw!k) = (-i, x-1)
      | otherwise = (i, x+i)


caught :: Delay -> (Int, Int) -> Bool
caught delay (depth, range) = (depth + delay) `mod` (2 * (range - 1)) == 0


anyCaught :: Firewall -> Delay -> Bool
anyCaught fw delay = any (caught delay) $ M.toList fw


day13 :: IO ()
day13 = do
  inLines <- getLines 13
  let fw :: Firewall
      fw = M.fromList $ parseLine <$> inLines
     
  putStrLn $ "Day13: part1: " ++ show (trip 0 fw)
  putStrLn $ "Day13: part2: " ++ show (findIndex not $ anyCaught fw <$> [0..])

  return ()



