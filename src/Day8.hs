
module Day8 where


import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map, insert, (!))

import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Utils
import Alg


type Reg = String
type Regs = Map Reg Int

data Op = Eq | Ne | Le | Ge | Lt | Gt

instance Show Op where
  show Eq = "=="
  show Ne = "!="
  show Gt = ">"
  show Lt = "<"
  show Ge = ">="
  show Le = "<="


data Ins = Inc String Int | Dec String Int deriving (Show, Eq)


i2r :: Ins -> Reg
i2r (Inc n _) = n
i2r (Dec n _) = n


parseIns :: String -> Ins
parseIns s = if (ws!!1) == "inc"
               then
                 Inc (head ws) (read $ ws!!2)
               else
                 Dec (head ws) (read $ ws!!2)
  where
    ws = words s

data Cond = Cond { reg :: String, op :: Op, v :: Int} deriving (Show)


parseCond :: String -> Cond
parseCond s = case ws!!1 of
                "==" -> Cond (head ws) Eq (read $ ws!!2)
                "!=" -> Cond (head ws) Ne (read $ ws!!2)
                ">" -> Cond (head ws) Gt (read $ ws!!2)
                ">=" -> Cond (head ws) Ge (read $ ws!!2)
                "<" -> Cond (head ws) Lt (read $ ws!!2)
                "<=" -> Cond (head ws) Le (read $ ws!!2)
                _ -> error $ "Can't read operator: " ++ ws!!1
  where
    ws = words s

data Line = Line Ins Cond deriving (Show)

parseLine :: String -> Line
parseLine s = Line (parseIns $ head ps) (parseCond $ ps!!1)
  where
    ps = splitOn " if " s


test :: Cond -> Regs -> Bool
test (Cond r c x) rs = 
  case c of 
    Eq -> v == x
    Ne -> v /= x
    Lt -> v < x
    Gt -> v > x
    Le -> v <= x
    Ge -> v >= x
  where
    v = M.findWithDefault 0 r rs

apply :: Ins -> Regs -> Regs
apply i rs = case i of
  Inc n x -> insert n (v + x) rs
  Dec n x -> insert n (v - x) rs
  where
    v = M.findWithDefault 0 (i2r i) rs


run :: [Line] -> Regs -> Regs
run [] rs = rs
run ((Line i c):ls) rs = if test c rs then run ls (apply i rs) else run ls rs

run2 :: [Line] -> (Int, Regs) -> (Int, Regs)
run2 [] rs = rs
run2 ((Line i c):ls) (m, rs) = run2 ls (newMx, newRs)
  where
    newRs = if test c rs then apply i rs else rs
    newMx = if test c rs then maximum (m : M.elems newRs) else m



day8 :: IO ()
day8 = do
  inLines <- getLines 8
  let ls = parseLine <$> inLines
     
  putStrLn $ "Day8: part1: " ++ show (maximum $ M.elems $ run ls M.empty)
  putStrLn $ "Day8: part2: " ++ show (fst $ run2 ls (0, M.empty))

  return ()


