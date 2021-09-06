
module Day12 where

import Data.Map (Map, (!), fromList)
import qualified Data.Map as M
import Data.List hiding (group)
import Data.Maybe

import Utils


parseLine :: String -> (Int, [Int])
parseLine s = (read $ head ps, read <$> ns)
  where
    ps = splitOn " <-> " s
    ns = splitOn ", " $ ps!!1


group :: Int -> Map Int [Int] -> [Int]
group x mp = go [] [x] 
  where
    go acc [] = acc
    go acc (x:xs) 
      | x `elem` acc = go acc xs
      | otherwise = go (x:acc) (nub $ xs ++ fromMaybe (error $ "Not in map: " ++ show x) (M.lookup x mp))


groups :: Map Int [Int] -> [[Int]]
groups = go []
  where
    go gs mp1 
      | null mp1 = gs
      | otherwise = go (g:gs) (foldr M.delete mp1 g)
                      where
                        g = getGroup mp1

    getGroup mp2 = group k mp2
      where
        k = head $ M.keys mp2


day12 :: IO ()
day12 = do
  inLines <- getLines 12
  let mp :: Map Int [Int]
      mp = fromList $ parseLine <$> inLines
     
  putStrLn $ "Day12: part1: " ++ show (length $ group 0 mp)
  putStrLn $ "Day12: part2: " ++ show (length $ groups mp)

  return ()


