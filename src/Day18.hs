{-# LANGUAGE BangPatterns, FlexibleContexts, LambdaCase, NamedFieldPuns, RecordWildCards, ViewPatterns #-}

module Day18 where


import TotalMap (TMap(..), (!), insert, empty)
import qualified Data.Map as M
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import Data.Maybe
import Data.List.PointedList (PointedList(..), moveN, next)
import qualified Data.List.PointedList as P
import Utils

import Debug.Trace


type Reg = Char


data Val = RVal Reg | IVal Int deriving (Show, Eq)


data Ins =  Snd Val 
          | Set Reg Val 
          | Add Reg Val 
          | Mul Reg Val 
          | Mod Reg Val 
          | Rcv Reg 
          | Jgz Val Val 
          deriving (Show, Eq)


parseVal :: String -> Val
parseVal [] = error "This shouldn't happen in parseVal"
parseVal s@(c:cs) = if c `elem` ['a'..'z'] then RVal c else IVal (read s)


parseIns :: String -> Ins
parseIns s
  | w0 == "snd" = Snd (parseVal $ ws!!1)
  | w0 == "set" = Set w1 (parseVal $ ws!!2)
  | w0 == "add" = Add w1 (parseVal $ ws!!2)
  | w0 == "mul" = Mul w1 (parseVal $ ws!!2)
  | w0 == "mod" = Mod w1 (parseVal $ ws!!2)
  | w0 == "rcv" = Rcv w1
  | w0 == "jgz" = Jgz (parseVal $ ws!!1) (parseVal $ ws!!2)
  | otherwise = error "This shouldn;t happen in parseIns"
  where
    w0 = head ws
    w1 = head $ ws!!1
    ws = words s


type Freq = Int


run1 :: PointedList Ins -> Maybe Freq
run1 l = go (TMap 0 empty) Nothing (Just l)
  where
    go :: TMap Char Int -> Maybe Freq -> Maybe (PointedList Ins) -> Maybe Freq
    go regs sound Nothing = Nothing
    go regs sound (Just pl) = 
      case _focus pl of
        Snd (RVal c)          -> go regs (Just $ regs!c) $ next pl
        Snd (IVal x)          -> go regs (Just x) $ next pl
        Set r (RVal c) -> go (insert r (regs!c) regs) sound $ next pl
        Set r (IVal x) -> go (insert r x regs) sound $ next pl
        Add r (RVal c) -> go (insert r ((regs!r) + regs!c) regs) sound $ next pl
        Add r (IVal x) -> go (insert r ((regs!r) + x) regs) sound $ next pl
        Mul r (RVal c) -> go (insert r ((regs!r) * regs!c) regs) sound $ next pl
        Mul r (IVal x) -> go (insert r ((regs!r) * x) regs) sound $ next pl
        Mod r (RVal c) -> go (insert r ((regs!r) `mod` regs!c) regs) sound $ next pl
        Mod r (IVal x) -> go (insert r ((regs!r) `mod` x) regs) sound $ next pl
        Rcv r -> if regs!r /=0 then sound else go regs sound $ next pl
        Jgz (RVal r) (RVal c) -> if regs!r > 0 then
                            go regs sound $ moveN (regs!c) pl
                          else
                            go regs sound $ next pl
        Jgz (RVal r) (IVal x) -> if regs!r > 0 then
                            go regs sound $ moveN x pl
                          else
                            go regs sound $ next pl
        Jgz (IVal r) (RVal c) -> if r > 0 then
                            go regs sound $ moveN (regs!c) pl
                          else
                            go regs sound $ next pl
        Jgz (IVal r) (IVal x) -> if r > 0 then
                            go regs sound $ moveN x pl
                          else
                            go regs sound $ next pl


data Proc = Proc {sentCount :: Int , send :: Seq Int, rec :: Seq Int, regs :: TMap Char Int, ins :: Maybe (PointedList Ins)} deriving (Show)


run2 :: Proc -> Proc
run2 p@(Proc _ _ _ _ Nothing) = p
run2 p@(Proc sc sn rc regs (Just pl)) = 
  case _focus pl of
    --Snd (RVal c) -> trace ("sending: " ++ show (regs!c)) $ go $ Proc (sc+1) (sn S.|> regs!c) rc regs $ next pl
  Snd (RVal c) -> run2 $ Proc (sc+1) (sn S.|> regs!c) rc regs $ next pl
  Snd (IVal x) -> run2 $ Proc (sc+1) (sn S.|> x) rc regs $ next pl
  Rcv x -> if null rc 
            then 
              p 
            else 
              let (r :<| rs) = rc
              in run2 $ Proc sc sn rs (insert x r regs) $ next pl
  Set r (RVal c) -> run2 $ Proc sc sn rc (insert r (regs!c) regs) $ next pl
  Set r (IVal x) -> run2 $ Proc sc sn rc (insert r x regs) $ next pl
  Add r (RVal c) -> run2 $ Proc sc sn rc (insert r ((regs!r) + regs!c) regs) $ next pl
  Add r (IVal x) -> run2 $ Proc sc sn rc (insert r ((regs!r) + x) regs) $ next pl
  Mul r (RVal c) -> run2 $ Proc sc sn rc (insert r ((regs!r) * regs!c) regs) $ next pl
  Mul r (IVal x) -> run2 $ Proc sc sn rc (insert r ((regs!r) * x) regs) $ next pl
  Mod r (RVal c) -> run2 $ Proc sc sn rc (insert r ((regs!r) `mod` regs!c) regs)$ next pl
  Mod r (IVal x) -> run2 $ Proc sc sn rc (insert r ((regs!r) `mod` x) regs) $ next pl
  Jgz (RVal r) (RVal c) -> if regs!r > 0 then
                      run2 $ Proc sc sn rc regs $ moveN (regs!c) pl
                    else
                      run2 $ Proc sc sn rc regs $ next pl
  Jgz (RVal r) (IVal x) -> if regs!r > 0 then
                      run2 $ Proc sc sn rc regs $ moveN x pl
                    else
                      run2 $ Proc sc sn rc regs $ next pl
  Jgz (IVal x) (RVal c) -> if x > 0 then
                      run2 $ Proc sc sn rc regs $ moveN (regs!c) pl
                    else
                      run2 $ Proc sc sn rc regs $ next pl
  Jgz (IVal y) (IVal x) -> if y > 0 then
                      run2 $ Proc sc sn rc regs $ moveN x pl
                    else
                      run2 $ Proc sc sn rc regs $ next pl


run :: PointedList Ins -> (Proc, Proc)
run ins = go p0 p1
  where
    start0 = Proc 0 S.empty S.empty (insert 'p' 0 $ TMap 0 empty) $ Just ins
    p0@(Proc _ s0 _ rg0 i0) = run2 start0
    start1 = Proc 0 S.empty s0 (insert 'p' 1 $ TMap 0 empty) $ Just ins
    p1 = run2 start1

    go :: Proc -> Proc -> (Proc, Proc)
    go in0@(Proc sc0 _ _ regs0 ins0) in1@(Proc sc1 sn1 _ regs1 ins1)
      | null sn1 = (in0, in1)
      | isNothing ins0 =  (in0, in1)
      | isNothing ins1 =  (in0, in1)
      | otherwise = go out0 out1
        where
          out0@(Proc _ s0 _ _ _) = run2 (Proc sc0 S.empty sn1 regs0 ins0)
          out1 = run2 (Proc sc1 S.empty s0 regs1 ins1)


day18 :: IO ()
day18 = do
  inLines <- getLines 18
  let ls :: PointedList Ins
      ls = fromMaybe (error "There should be one element") $ P.fromList $ parseIns <$> inLines

  putStrLn $ "Day18: part1: " ++ show (run1 ls)
  putStrLn $ "Day18: part2: " ++ show (sentCount $ snd $ run ls)
    
  return ()



