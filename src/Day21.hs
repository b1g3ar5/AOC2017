{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Day21 where

import Data.List (minimumBy, sortOn, groupBy, elem, intercalate, transpose)
import Data.List.Split ( splitOn, chunksOf )
import Data.Ord
import Data.Maybe
import Data.Semigroup.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Bifunctor
import Utils hiding (above)

import System.TimeIt


type Pattern = [String]
type  Rules = Map Pattern Pattern


parsePattern :: String -> Pattern
parsePattern = splitOn "/"


parseRule :: String -> (Pattern, Pattern)
parseRule s = (parsePattern $ head ps, parsePattern $ ps!!1)
  where
    ps = splitOn " => " s


-- I think this is all the possibilities
expand :: [(Pattern, Pattern)] -> [(Pattern, Pattern)]
expand rs = first <$> [id
                      , reverse
                      , (reverse <$>)
                      , transpose
                      , reverse . transpose
                      , reverse . (reverse <$>)
                      , transpose . reverse
                      , transpose . reverse . (reverse <$>)
                      ] <*> rs



split :: Pattern -> [[Pattern]]
split ls
  | n == 2 = ss 2
  | n == 3 = ss 3
  | even n = ss 2
  | n `mod` 3 == 0 = ss 3
  | otherwise  = error "This shouldn't happen in split"
  where
    n = length ls
    ss :: Int -> [[Pattern]]
    ss m = (transpose <$>) <$> chunksOf m $ chunksOf m <$> ls


unsplit :: [[Pattern]] -> Pattern
unsplit pss = concat $ foldl1 (zipWith (++)) <$> pss


start :: Pattern
start = [".#.", "..#", "###"]


-- Give an error message if the pattern is not in the map
lu :: (Show k, Ord k) => Map k a -> k -> a
lu m k = fromMaybe (error $ show k ++ " is not in the map") $ M.lookup k m


go :: Rules -> Int -> Pattern -> Int
go _ 0 s = count s
go rs n s
  | length s == 3 = go rs (n-1) $ unsplit $ (lu rs <$>) <$> pss
  -- If the sub patterns are 3x3 we can call go on them and add up the results
  | length (head $ head pss) == 3  = sum $ concat $ (go rs n <$>) <$> pss
  | otherwise = go rs  (n-1) $ unsplit $ (lu rs <$>) <$> pss
  where
    pss = split s


count :: Pattern -> Int
count p = length $ filter (=='#') $ concat p


day21 :: IO ()
day21 = do
  inLines <- getLines 21
  let rs :: [(Pattern, Pattern)]
      rs = parseRule <$> inLines
      rules :: Rules
      rules = M.fromList $ expand rs

  putStrLn $ "Day21: part1:go: " ++ show (go rules 5 start)
  putStrLn $ "Day21: part1:go: " ++ show (go rules 18 start)
   
  return ()


