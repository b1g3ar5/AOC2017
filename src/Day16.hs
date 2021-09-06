
module Day16 where

import Utils

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)

start, test :: String
start = "abcdefghijklmnop"
test = "abcde"

testMoves = [Spin 4, Exchange 3 4, Partner 'e' 'b' ]

apply :: Move -> String -> String
apply (Spin x) s = drop x s ++ take x s
apply (Exchange i j) s = (\(ix, c) -> if ix==i then s!!j else if ix==j then s!!i else c) <$> zip [0..] s
apply (Partner x y) s = (\c -> if c==x then y else if c==y then x else c) <$> s

parseMove :: String -> Move
parseMove s
  | c=='s' = Spin $ (16 - read (tail s))
  | c=='x' = Exchange (read $ head ps) (read $ ps!!1)
  | c=='p' = Partner  (head $ head ps) (head $ ps!!1)
  | otherwise = error $ "This shouldn't happen: " ++ s
  where
    c = head s
    ps = splitOn "/" $ tail s


-- Works out how many repeats to get back to the start position
toStart :: [Move] -> String -> Int
toStart [] s = error "This shouldn't happen"
toStart (m:ms) s = go 1 (ms ++ concat (repeat $ m:ms)) $ apply m s
  where
    go n [] t = n
    go n (d:ds) t = if t==start then
                      n
                    else
                      go (n+1) ds (apply d t)



day16 :: IO ()
day16 = do
  inLines <- getLines 16
  let ms :: [Move]
      ms = parseMove <$> splitOn "," (head inLines)
      n = length ms

      backToStart = 1000000000 `mod` fromIntegral (toStart ms start)
      repeats = backToStart `div` n
     
  putStrLn $ "Day16: part1: " ++ show (foldl (flip apply) start ms)
  putStrLn $ "Day16: part2: " ++ show (foldl (flip apply) start $ concat $ replicate repeats ms)

  return ()


