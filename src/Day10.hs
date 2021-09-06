
module Day10 where

import Data.Char ( ord )
import Data.List.Split ( chunksOf, splitOn )
import Data.Bits ( Bits(xor) )
import Numeric ( showHex )
import Utils ( splitOn, getLines )


standard :: Ring
standard = Ring 0 0 [0..(ringSize-1)]

ringSize :: Int
ringSize = 256


data Ring = Ring {focus :: Int, skip :: Int, dat :: [Int]} deriving (Show)


move :: [Int] -> Ring -> Ring
move [] r = r
move (m:ms) r@(Ring f s ns) = move ms $ Ring newFocus (s+1) $ p2 ++ p1
  where
    -- Reverse the front m numbers
    new = reverse (take m ns) ++ drop m ns
    -- Rotate to the new focus
    (p1, p2) = splitAt ((m+s) `mod` ringSize) new
    newFocus = (f + m + s) `mod` ringSize


check1 :: Ring -> Int
check1 (Ring f s ns) = ms!!0 * ms!!1
  where
    ms = drop (ringSize - f) ns ++ take (ringSize - f) ns


suffix :: [Int]
suffix = [17, 31, 73, 47, 23]


xors :: [Int] -> Int
xors [] = error "shouldn;t get here in xors"
xors [x] = x
xors (x:xs) = x `xor` xors xs


knotHash :: String -> String
knotHash str = foldr f2 "" $ showHex . xors <$> chunksOf 16 ms
  where
    ps = (ord <$> str) ++ suffix
    Ring fs s xs = move (concat $ replicate 64 ps) standard
    ms = drop (ringSize - fs) xs ++ take (ringSize - fs) xs
    -- We need to pad numbers less than 16 (ie single hex digit)
    f2 :: (String -> String) -> String -> String
    f2 f s = if even (length out) then out else '0':out
      where
        out = f s


day10 :: IO ()
day10 = do
  inLines <- getLines 10
  let input = head inLines
      ms :: [Int]
      ms = read <$> splitOn "," input
      r1 = move ms standard
           
  putStrLn $ "Day10: part1: " ++ show (check1 r1)
  putStrLn $ "Day10: part2: " ++ knotHash input

  return ()


