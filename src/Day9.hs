
module Day9 where

import Utils

purge :: String -> (Int, String)
purge = go (0, [])
  where
    go :: (Int, String) -> String -> (Int, String)
    go (n, acc) [] = (n, acc)
    go (n, acc) (c:cs) = 
      case c of
        '!' -> go (n, acc) $ tail cs
        '<' -> go (m, acc) rem
                 where
                   (m, rem) = findRight (n, cs)
        _   -> go (n, acc++[c]) cs
    
    findRight :: (Int, String) -> (Int, String)
    findRight (_, []) = error "Didn't find >"
    findRight (n, c:cs) = case c of
                         '!' -> findRight (n, tail cs)
                         '>' -> (n, cs)
                         _   -> findRight (n+1, cs)


score :: String -> Int
score [] = 0
score s = go (0,0) s
  where
    go (n,l) [] = n
    go (n,l) (c:cs) = 
      case c of
        '{' -> go (n, l+1) cs
        '}' -> go (n+l, l-1) cs
        _   -> go (n, l) cs



day9 :: IO ()
day9 = do
  inLines <- getLines 9
  let ln = head inLines
     
  putStrLn $ "Day9: part1: " ++ show (score $ snd $ purge ln)
  putStrLn $ "Day9: part2: " ++ show (fst $ purge ln)

  return ()


