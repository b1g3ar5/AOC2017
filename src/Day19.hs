
module Day19 where


import Data.Map (Map)
import qualified Data.Map as M
import TotalMap (TMap(..), (!))
import qualified TotalMap as T
import Utils

data Cell = Vert | Horiz | Cross | Letter Char | Empty deriving (Show, Eq)

parseCell :: Char -> Cell
parseCell ' ' = Empty
parseCell '|' = Vert
parseCell '-' = Horiz
parseCell '+' = Cross
parseCell c = Letter c


type Grid = TMap Coord Cell

parseGrid :: [String] -> Grid
parseGrid ls = TMap Empty $ M.fromList $ filter (\(cd, c) -> c /= Empty) $ concat $ (\(row, cs) -> (\(col, c) -> ((col, row), parseCell c)) <$> zip [0..] cs) <$> zip [0..] ls


turn90 :: (Int, Int) -> (Int, Int)
turn90 (0, 1) = (-1, 0)
turn90 (0, -1) = (1, 0)
turn90 (1, 0) = (0, 1)
turn90 (-1, 0) = (0, -1)
turn90 x = error $ "This houdln't happen: " ++ show x

path :: Grid -> (String, Int)
path g = go 0 [] (1, 0) (0, 1)
  where
    go :: Int -> String -> Coord -> Coord -> (String, Int)
    go n acc pos dir = 
      case g!pos of
        Vert -> go (n+1) acc (pos + dir) dir
        Horiz -> go (n+1) acc (pos + dir) dir
        Cross -> if g!(pos + dir) /= Empty
                   then 
                     go (n+1) acc (pos + dir) dir
                   else 
                     if g!(pos + turn90 dir) /= Empty
                       then 
                        go (n+1) acc (pos + turn90 dir) (turn90 dir)
                      else
                        go (n+1) acc (pos - turn90 dir) (- (turn90 dir))
        Letter c -> go (n+1) (acc ++ [c]) (pos + dir) dir
        Empty -> (acc, n)
                     



day19 :: IO ()
day19 = do
  inLines <- getLines 19
  let grid :: TMap Coord Cell
      grid = parseGrid inLines
     
  putStrLn $ "Day19: " ++ show (path grid)

  return ()


