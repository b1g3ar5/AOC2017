
module Day2 where


import Utils


mqr :: Int -> Int -> Maybe Int
mqr x y = if r1==0 then Just q1 else (if r2==0 then Just q2 else Nothing)
  where
    (q1, r1) = quotRem x y
    (q2, r2) = quotRem y x


find2 :: [Int] -> Int
find2 [] = error "Shouldn't be able to get here in find2"
find2 (x:xs) = case go xs of
                 Just q -> q
                 Nothing -> find2 xs
  where
    go [] = Nothing
    go (y:ys) = case mqr x y of 
                  Just q -> Just q
                  Nothing -> go ys


address = 325489
{-
325489 = 570^2 + 589
Looking at squares
4 = 1 step
9 = 2 steps
16 = 3 steps
...
570^2 = 569 steps

Now the next side takes 570 numbers, so we have 19 left.
The first cell of the side takes 570 steps, so the last will need 571 steps.
The next 19 numbers reduce the steps so the answer is

571 - 19 = 552
-}

day2 :: IO ()
day2 = do
  inLines <- getLines 2
  let xss :: [[Int]]
      xss = (read <$>) . words <$> inLines
  putStrLn $ "Day2: part1: " ++ show (sum $ (\ns -> maximum ns - minimum ns) <$> xss)
  putStrLn $ "Day2: part2: " ++ show (sum $ find2 <$> xss)

  return ()


