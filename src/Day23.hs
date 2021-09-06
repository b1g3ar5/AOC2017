{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}


module Day23 where

import Data.List.PointedList (PointedList(..), moveN, next)
import Data.Maybe
import qualified Data.List.PointedList as P
import Utils
import TotalMap hiding (filter, null)


type Reg = Char


data Val = RVal Reg | IVal Int deriving (Show, Eq)


data Ins =  Set Reg Val 
          | Sub Reg Val 
          | Mul Reg Val 
          | Jnz Val Val 
          deriving (Show, Eq)


parseVal :: String -> Val
parseVal [] = error "This shouldn't happen in parseVal"
parseVal s@(c:cs) = if c `elem` ['a'..'z'] then RVal c else IVal (read s)


parseIns :: String -> Ins
parseIns s
  | w0 == "set" = Set w1 (parseVal $ ws!!2)
  | w0 == "sub" = Sub w1 (parseVal $ ws!!2)
  | w0 == "mul" = Mul w1 (parseVal $ ws!!2)
  | w0 == "jnz" = Jnz (parseVal $ ws!!1) (parseVal $ ws!!2)
  | otherwise = error "This shouldn;t happen in parseIns"
  where
    w0 = head ws
    w1 = head $ ws!!1
    ws = words s


type Freq = Int


run1 :: TMap Char Int ->PointedList Ins -> Int
run1 m l = go m 0 (Just l)
  where
    go :: TMap Char Int -> Int -> Maybe (PointedList Ins) -> Int
    go _ n Nothing = n
    go regs n (Just pl) = 
      case _focus pl of
        Set r (RVal c) -> go (insert r (regs!c) regs) n $ next pl
        Set r (IVal x) -> go (insert r x regs) n $ next pl
        Sub r (RVal c) -> go (insert r ((regs!r) - regs!c) regs) n $ next pl
        Sub r (IVal x) -> go (insert r ((regs!r) - x) regs) n $ next pl
        Mul r (RVal c) -> go (insert r ((regs!r) * regs!c) regs) (n+1) $ next pl
        Mul r (IVal x) -> go (insert r ((regs!r) * x) regs) (n+1) $ next pl
        Jnz (RVal r) (RVal c) -> if regs!r /= 0 then
                            go regs n $ moveN (regs!c) pl
                          else
                            go regs n $ next pl
        Jnz (RVal r) (IVal x) -> if regs!r /= 0 then
                            go regs n $ moveN x pl
                          else
                            go regs n $ next pl
        Jnz (IVal r) (RVal c) -> if r /= 0 then
                            go regs n $ moveN (regs!c) pl
                          else
                            go regs n $ next pl
        Jnz (IVal r) (IVal x) -> if r /= 0 then
                            go regs n $ moveN x pl
                          else
                            go regs n $ next pl


run2 :: Int -> TMap Char Int ->PointedList Ins -> TMap Char Int
run2 count m l = go count m 0 (Just l)
  where
    go :: Int -> TMap Char Int -> Int -> Maybe (PointedList Ins) -> TMap Char Int
    go 0 regs n _ = regs
    go _ regs n Nothing = regs
    go count regs n (Just pl) = 
      case _focus pl of
        Set r (RVal c) -> go (count-1) (insert r (regs!c) regs) n $ next pl
        Set r (IVal x) -> go (count-1) (insert r x regs) n $ next pl
        Sub r (RVal c) -> go (count-1) (insert r ((regs!r) - regs!c) regs) n $ next pl
        Sub r (IVal x) -> go (count-1) (insert r ((regs!r) - x) regs) n $ next pl
        Mul r (RVal c) -> go (count-1) (insert r ((regs!r) * regs!c) regs) (n+1) $ next pl
        Mul r (IVal x) -> go (count-1) (insert r ((regs!r) * x) regs) (n+1) $ next pl
        Jnz (RVal r) (RVal c) -> if regs!r /= 0 then
                            go (count-1) regs n $ moveN (regs!c) pl
                          else
                            go (count-1) regs n $ next pl
        Jnz (RVal r) (IVal x) -> if regs!r /= 0 then
                            go (count-1) regs n $ moveN x pl
                          else
                            go (count-1) regs n $ next pl
        Jnz (IVal r) (RVal c) -> if r /= 0 then
                            go (count-1) regs n $ moveN (regs!c) pl
                          else
                            go (count-1) regs n $ next pl
        Jnz (IVal r) (IVal x) -> if r /= 0 then
                            go (count-1) regs n $ moveN x pl
                          else
                            go (count-1) regs n $ next pl


isPrime k = (k > 1) && null [ x | x <- [2..k - 1], k `mod` x == 0] 


day23 :: IO ()
day23 = do
  inLines <- getLines 23
  let ls :: PointedList Ins
      ls = fromMaybe (error "There should be one element") $ P.fromList $ parseIns <$> inLines
     
  putStrLn $ "Day23: part1: " ++ show (run1 (TMap 0 empty) ls)
  putStrLn $ "Day23: part2: " ++ show ( length $ filter (not . isPrime)  [109900, 109917.. 126900])

  return ()


{-

b = 99
c = b
b = b * 100
b = b + 100000
c = b + 17000
f = 1
d = 2

e = 2
g=1
until g == 0
{ g = d * e - b
  if g == 0 then f = 0
  e = e + 1
  g  = e - b
}
d = d + 1
g = d - b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23


-}