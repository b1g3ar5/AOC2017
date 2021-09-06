
module Day4 where


import Data.List
import Utils


valid1 :: [String] -> Bool
valid1 ss = length (nub ss) == length ss

valid2 :: [String] -> Bool
valid2 ss = length (nub sss) == length sss
  where
    sss = sort <$> ss


day4 :: IO ()
day4 = do
  inLines <- getLines 4
  let ls = words <$> inLines
     
  putStrLn $ "Day4: part1: " ++ show (length $ filter id $ valid1 <$> ls)
  putStrLn $ "Day4: part2: " ++ show (length $ filter id $ valid2 <$> ls)

  return ()


