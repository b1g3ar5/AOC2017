{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module
 Day24 where

import Data.Tuple
import Data.List
import Control.Monad
import Utils
import Alg hiding (filter, zip)


type Domino = (Int, Int)


coalg :: Coalgebra (TreeF Domino) (Domino, [Domino])
coalg (dom@(domf, doms), pool)
  | null possibles = TreeF dom []
  | otherwise = TreeF dom [(if df == doms then d else swap d, delete d pool) | d@(df, ds) <- possibles]
  where
    possibles = filter (\(f,s) -> f == doms || s == doms) pool


palg :: Algebra (TreeF Domino) String
palg (TreeF d@(df, ds) []) = "Leaf: " ++ "(" ++ show df ++ ", " ++ show ds ++ ")"
palg (TreeF d@(df, ds) lst) = "Node: " ++  "(" ++ show df ++ ", " ++ show ds ++ ")" ++ " and: " ++ show (filter (/='"') <$> lst)


strengths :: Algebra (TreeF Domino) Int
strengths (TreeF d@(df, ds) []) = df+ds
strengths (TreeF d@(df, ds) lst) = df + ds + maximum lst


lengths :: Algebra (TreeF Domino) (Int, Int)
lengths (TreeF d@(df, ds) []) = (1, df + ds)
lengths (TreeF d@(df, ds) lss) = (l+1, df + ds + s)
  where
    (l, s) = maximum lss



parseDomino :: String -> Domino
parseDomino s = (read $ head ps, read $ ps!!1)
  where
    ps = splitOn "/" s


day24 :: IO ()
day24 = do
  inLines <- getLines 24
  let ds :: [Domino]
      ds = parseDomino <$>inLines
      starts :: [Domino]
      starts = [(0, 7), (0, 35), (0, 22)]
      pools :: [[Domino]]
      pools = (`delete` ds) <$> starts
      go f = maximum $ (\(s, p) -> hylo f coalg (s, p))  <$> zip starts pools
     
  putStrLn $ "Day24: part1: " ++ show (go strengths)
  putStrLn $ "Day24: part2: " ++ show (snd $ go lengths)
  
  return ()


