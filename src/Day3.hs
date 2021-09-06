
module Day3 where


import Utils
import TotalMap

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


data Dir = Up | Dn | Rt | Lt
type Level = Int
type State = (Level, Dir, Coord, Int)

move :: State -> State
move (n, Rt, p, 1) = (n, Up, p + (1, 0), n)
move (n, Rt, p, m) = (n, Rt, p + (1, 0), m-1)
move (n, Up, p, 1) = (n+1, Lt, p + (0, -1), n+1)
move (n, Up, p, m) = (n, Up, p + (0, -1), m-1)
move (n, Lt, p, 1) = (n, Dn, p + (-1, 0), n)
move (n, Lt, p, m) = (n, Lt, p + (-1, 0), m-1)
move (n, Dn, p, 1) = (n+1, Rt, p + (0, 1), n+1)
move (n, Dn, p, m) = (n, Dn, p + (0, 1), m-1)

type Grid1 = TMap Int Coord
type Grid2 = TMap Coord Int

fillGrid1 :: Int -> Grid1
fillGrid1 target = snd $ go 1 ((1, Rt, (0,0), 1), TMap 0 empty) 
  where
    go :: Int -> (State, Grid1) -> (State, Grid1)
    go n (s@(l, d, p, i), g) = 
      if n == target then 
        (s, g)
        else
          go (n+1) (move s, insert n p g)


fillGrid2 :: Int -> Grid2
fillGrid2 target = snd $ go ((1, Rt, (0,0), 1), insert (0, 0) 1 $ TMap 0 empty) 
  where
    go :: (State, Grid2) -> (State, Grid2)
    go (s@(l, d, p, i), g) = 
      if n > target then 
        (s, insert p n g)
        else
          go (move s, insert p (if n==0 then 1 else n) g)
      where
        ns :: [Coord]
        ns = neighbours8 p
        n = sum $ (g !) <$>  ns


day3 :: IO ()
day3 = do
  inLines <- getLines 2
  let xss :: [[Int]]
      xss = (read <$>) . words <$> inLines
      g1 = fillGrid1 (address+1)
      g2 = fillGrid2 address
     
  putStrLn $ "Day3: part1: " ++ show (manhattan (0, 0) $ g1 ! address)
  putStrLn $ "Day3: part2: " ++ show (maximum $ snd <$> assocs g2)

  return ()


