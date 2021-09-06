{-# LANGUAGE FlexibleContexts, GADTs, TypeFamilies #-}

module Day14 where


import Data.Graph (stronglyConnComp)
import Utils ( isNeighbour4, Coord, neighbours4 )
import Day10 ( knotHash )


{-

I tried to do this with Comonadsheet (https://github.com/kwf/ComonadSheet) but
I couldn't get the groups to form without looping which meant that evaluate (ie. fix)
wouldn't work. I Conway's game of life the state of a cell depends on the state of
the neighbouring cells at the preceeding time. With groups I couldn't work out an algorithm
like that.

So... I resorted to the one line using Data.Graph

-}

toBinary :: Char -> String
toBinary '0' = "0000"
toBinary '1' = "0001"
toBinary '2' = "0010"
toBinary '3' = "0011"
toBinary '4' = "0100"
toBinary '5' = "0101"
toBinary '6' = "0110"
toBinary '7' = "0111"
toBinary '8' = "1000"
toBinary '9' = "1001"
toBinary 'a' = "1010"
toBinary 'b' = "1011"
toBinary 'c' = "1100"
toBinary 'd' = "1101"
toBinary 'e' = "1110"
toBinary 'f' = "1111"
toBinary c = error $ "This shouldn't be possible in toBinary: " ++ [c]


bshow :: String -> String
bshow s = (\c -> if c == '1' then '#' else '.') <$> s


count :: [String] -> Int
count ss = length $ filter (== '1') $ concat ss


toCoords :: [String] -> [Coord]
toCoords xss = fst <$> filter (\t-> snd t == '1') (concat ((\(r, xs) -> (\(c, x) -> ((c, r), x)) <$> zip [0..] xs) <$> zip [0..] xss))


toGroups :: [Coord] -> [[Coord]]
toGroups = go []
  where
    go :: [[Coord]] -> [Coord] -> [[Coord]]
    go gs [] = gs
    go gs (c:cs) = go (ins:gs) outs
      where
        (ins, outs) = collect c cs


-- Works out a group from a coord
collect :: Coord -> [Coord] -> ([Coord], [Coord])
collect c = go [] [c]
  where
    go gp toAdd [] = (gp ++ toAdd, [])
    go gp [] ps = (gp, ps)
    go gp (a:as) pool = go (a:gp) (ns ++ as) notns
      where
        -- add the neighbours to the toAdd list
        (ns, notns) = splitWith (isNeighbour4 a) pool


splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith p xs = go ([],[]) xs
  where
    --go :: ([a], [a]) -> [a] -> ([a], [a])
    go (ys, ns) [] = (ys, ns)
    go (ys, ns) (x:xs)
      | p x = go (x:ys, ns) xs
      | otherwise = go (ys, x:ns) xs


day14 :: IO ()
day14 = do
  let pwd = "wenycdww"
      test = "flqrgnkx"
      rs :: [String]
      rs = concat . (toBinary <$>) . knotHash . ((pwd ++) . ('-':) . show) <$> [0..127]
      cs = toCoords rs
      scc = stronglyConnComp $ (\c -> ('1', c, neighbours4 c)) <$> cs
    
  putStrLn $ "Day14: part1:" ++ show (length cs)
  putStrLn $ "Day14: part1:" ++ show (length scc)
  --putStrLn $ "Day14: part2:" ++ show (length $ toGroups cs)
    
  return ()


{-

Example - let's do the 1D version with lists

input = [1,1,1,0,0,0,1,1,0]  -- How many groups?

My idea: 

change to: 

ns = [1,2,3,0,0,0,7,8,0]

apply maximum neighbour if not zero to this: [3,3,3,0,0,0,8,8,0]

count groups = 2.

Now what are the functions in the: List (List Int -> Int)?

We need to work out the minimum of neighbours and the cell itself

xs, ns :: [Int]
xs = [0,1,1,1,1,0,1,1,0]
ns = zipWith (\x y -> if x==1 then y else 0) xs [1..]


-- Takes a list and returns
fs :: (Ord a, Eq a, Num a) => [a] -> [[a] -> a]
fs xs = (\(ix, x) ys -> maximum $ lst ix x ys) <$> zip [0..] xs
  where
    lst i p qs
      | p == 0 = [p]
      | i == 0  = [p, qs!!1]
      | i == (n - 1) = [p, qs!!(n-2)]
      | otherwise = [p, qs!!(i-1), qs!!(i+1)]
      where 
        n = length qs

q1 :: [Int] -> [Int]
q1 xs = map ($ xs) (fs xs)

Solution: 

part2 = fix q1

Unfortunately this <<loops>>.

-}    