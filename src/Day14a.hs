{-# LANGUAGE ViewPatterns #-}

module Day14a (day14a, day14b) where

import Day10 (knotHash)
import Disjoints (Disjoints(..), disjoint)
import Data.Ix                (index, range)
import Text.Printf            (printf)
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Text as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack

mkGrid :: String -> [[Bool]]
mkGrid (strip->k) = map mkRow [0..127]
  where
    mkRow :: Int -> [Bool]
    mkRow n = map (== '1') . concatMap (printf "%08b") . knotHash
            $ k ++ "-" ++ show n

test = "flqrgnkx"

day14a :: IO ()
day14a = putStrLn $ "Day14a: part1: " ++ show (length . filter id . concat . mkGrid $ test )

day14b :: IO ()
day14b = putStrLn $ "Day14a: part2: " ++ show (S.size . getD . litGroups . mkGrid $ test)

litGroups :: [[Bool]] -> Disjoints
litGroups grid = foldMap go (range r)
  where
    r = ((0,0),(127,127))
    isLit (x,y) = grid !! y !! x
    go p | isLit p   = disjoint . IS.fromList
                     . map (index r) . (p:) . filter isLit
                     $ neighbors p
         | otherwise = mempty

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [ (x+dx, y+dy) | (dx, dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
                                 , inBounds (x + dx) && inBounds (y + dy)
                  ]
  where
    inBounds z = z >= 0 && z < 128