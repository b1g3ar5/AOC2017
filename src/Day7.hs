
module Day7 where


import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Utils
import Alg


type Weight = Int 
type Name = String
type Label = (Name, Weight)


parseWeight :: String -> Weight
parseWeight s = read $ L.filter (\c -> (c /= '(') && (c /= ')')) s


parseLine :: String -> (Name, (Weight, [Name]))
parseLine s = (head ws, (parseWeight $ ws!!1, labels))
  where
    ps = splitOn " -> " s
    ws = words $ head ps
    labels = if L.length ps == 1 then [] else splitOn ", " $ ps!!1



coalg :: Coalgebra (TreeF Label) (Name, M.Map Name (Weight, [Name]))
coalg (nm, pool) = 
  case M.lookup nm pool of
    Nothing -> error "The name should be in the pool!"
    Just (wt, ns) -> TreeF (nm, wt) [(n, M.delete nm pool) | n <- ns]


alg :: Algebra (TreeF Label) (Either String Label)
alg (TreeF lb@(nm, wt) []) = Right lb
alg (TreeF lb@(nm, wt) lst) = 
  if null ls 
    then
      if allTheSameWith (\l m -> snd l == snd m) rs then
        Right (nm, wt + sum (snd <$> rights lst))
      else
        Left $ show $ uniqueWith (\x y -> snd x == snd y) rs 
    else
      Left $ head ls
  where
    rs = rights lst
    ls = lefts lst

palg :: Algebra (TreeF Label) String
palg (TreeF lb@(nm, wt) []) = "Leaf: " ++ "(" ++ nm ++ ", " ++ show wt ++ ")"
palg (TreeF lb@(nm, wt) lst) = "Node: " ++  "(" ++ nm ++ ", " ++ show wt ++ ")" ++ " and: " ++ show lst

walg :: Algebra (TreeF Label) (Either Label Label)
walg (TreeF lb@(nm, wt) []) = Right lb
walg (TreeF lb@(nm, wt) lst) = 
  if null ls 
    then 
      if allTheSameWith (\x y -> snd x == snd y) rs
        then
          Right (nm, wt + sum (snd <$> rs))
        else
          Left (fst (fromMaybe (error "This shouldn't happen in walg") oddOneOut), snd aNormalOne - snd (fromJust oddOneOut))
    else
      Left $ head ls
  where
    rs :: [Label]
    rs = rights lst
    ls :: [Label]
    ls = lefts lst
    oddOneOut = uniqueWith (\x y -> snd x == snd y) rs
    aNormalOne = head $ L.filter (\x -> snd x /= snd (fromJust oddOneOut)) rs



allTheSameWith  :: (a -> a -> Bool) -> [a] -> Bool
allTheSameWith _ [] = True
allTheSameWith _ [x] = True
allTheSameWith f xs = all (f (head xs)) (tail xs)


-- Assumes there is a uinique element
uniqueWith :: (a -> a -> Bool) -> [a] -> Maybe a
uniqueWith _ [] = Nothing
uniqueWith _ [x] = Just x
uniqueWith f (x:xs) = if null ys then 
                    Just x 
                  else 
                    uniqueWith f ys
  where
    ys = L.filter (f x) xs


testLines :: [String]
testLines = ["pbga (66)"
  , "xhth (57)"
  , "ebii (61)"
  , "havc (66)"
  , "ktlj (57)"
  , "fwft (72) -> ktlj, cntj, xhth"
  , "qoyq (66)"
  , "padx (45) -> pbga, havc, qoyq"
  , "tknk (41) -> ugml, padx, fwft"
  , "jptl (61)"
  , "ugml (68) -> gyxo, ebii, jptl"
  , "gyxo (61)"
  , "cntj (57)"]


day7 :: IO ()
day7 = do
  inLines <- getLines 7
  --let inLines = testLines
  let ls :: [(Name, (Weight, [Name]))]
      ls = parseLine <$> inLines
      allLabels = L.filter (\g -> L.length g == 1) $ L.group $ L.sort $ concat $ (\(n, (w, ns)) -> n:ns) <$> ls
      m :: M.Map Name (Weight, [Name])
      m = M.fromList ls
      h :: Label
      h@(n, w) = either id id $ hylo walg coalg ("dgoocsw", m)
      oldh@(ow, _) = m M.! n
     
  putStrLn $ "Day7: part1: " ++ show allLabels
  putStrLn $ "Day7: part2: " ++ show (n, ow + w)

  return ()


