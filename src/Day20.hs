{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Day20 where

import Data.List (minimumBy, sortOn, groupBy)
import Data.Ord
import Utils (getLines, splitOn)

type Vec = (Int, Int, Int)


instance Num Vec where
  (x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
  (x1, y1, z1) - (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
  (x1, y1, z1) * (x2, y2, z2) = undefined
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger i = (fromInteger i, 0, 0)


--p=<1609,-863,-779>
parseVec :: String -> Vec
parseVec s = (read (cs!!0), read (cs!!1), read (cs!!2))
  where
    cs = splitOn "," $ drop 3 s

data P = P {pid :: Int, pos::Vec, vel::Vec, acc::Vec} deriving (Show)

parseP :: (Int, String) -> P
parseP (n, s) = P n (parseVec $ ps!!0) (parseVec $ ps!!1) (parseVec $ ps!!2) 
  where
    ps = splitOn ", " $ filter (/='>') s


manhattan :: P -> Int
manhattan (P i (x,y,z) _ _) = abs x + abs y + abs z

update :: P -> P
update (P i p v a) = P i (p+v') v' a
  where
    v' = v + a


go1 :: [P] -> [P]
go1 ps = update <$> ps

go2 :: [P] -> [P]
go2 ps = (head <$>) <$> filter (\g -> length g == 1) $ groupBy (\x y -> pos x == pos y) $ sortOn pos $ update <$> ps


test = ["p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
  , "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
  , "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
  , "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"]

day20 :: IO ()
day20 = do
  inLines <- getLines 20
  let ps :: [P]
      ps = parseP <$> zip [0..] inLines
     
  putStrLn $ "Day20: part1: " ++ show (pid $ minimumBy (comparing manhattan) $ iterate go1 ps !! 1000)
  putStrLn $ "Day20: part2: " ++ show (length $ iterate go2 ps !!100)

  return ()


