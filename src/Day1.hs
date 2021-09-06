
module Day1 where


import Utils


capatcha2 :: [Int] -> Int
capatcha2 xs = sum $ zipWith (\x1 x2 -> if x1==x2 then 2*x1 else 0) p1 p2
  where
    (p1, p2) = splitAt (length xs `div` 2) xs


capatcha1 :: [Int] -> Int
capatcha1 [] = 0
capatcha1 (x:xs) = go (x, 0) (xs ++ [x])
  where
    go _ [] = error "Shouldn't be able to get here in capatcha"
    go (last, acc) [x] = if last == x then x+acc else acc
    go (last, acc) (x:xs) = go (x, if last==x then x+acc else acc) xs


day1 :: IO ()
day1 = do
  inWords <- getWords 1
  let xs :: [Int]
      xs = read . (:[]) <$> head inWords
  putStrLn $ "Day1: part1: " ++ show (capatcha1 xs)
  putStrLn $ "Day1: part2: " ++ show (capatcha2 xs)

  return ()


