
module Day11 where


import Utils
import Data.Data (Data(toConstr))

data Hex = N | S | NE | SE | NW | SW deriving (Eq, Show)

parseHex :: String -> Hex
parseHex "n" = N
parseHex "s" = S
parseHex "ne" = NE
parseHex "nw" = NW
parseHex "se" = SE
parseHex "sw" = SW
parseHex x = error $ "This shouldn't be sent tp parseHex" ++ x


hexDist :: Coord -> Int
hexDist (x, y) = (y - abs x) `div` 2 + abs x

toCoord :: Hex -> Coord
toCoord N = (0, -2)
toCoord S = (0, 2)
toCoord NE = (1, -1)
toCoord NW = (-1, -1)
toCoord SE = (1, 1)
toCoord SW = (-1, 1)


hexMax :: [Coord] -> Int
hexMax cs = go 0 (0, 0) cs
  where
    go :: Int -> Coord -> [Coord] -> Int
    go m p [] = m
    go m p (c:cs) = go (maximum [m, hexDist $ p+c]) (p+c) cs


day11 :: IO ()
day11 = do
  inLines <- getLines 11
  --let inLines = testLines
  let ls :: [Hex]
      ls = parseHex <$> splitOn "," (head inLines)
     
  putStrLn $ "Day11: part1: " ++ show (hexDist $ sum $ toCoord <$> ls)
  putStrLn $ "Day11: part2: " ++ show (hexMax $ toCoord <$> ls)

  return ()


