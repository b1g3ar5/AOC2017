{-# LANGUAGE TupleSections, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}


module Day25 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split

import TotalMap (TMap(..))
import qualified TotalMap as T
import System.TimeIt
import Utils
import Tape


-- This is all for mempty in moveR and moveL
instance Semigroup Bool where
  False <> False = False
  False <> True = True
  True <> False = True
  True <> True = True

-- I just wanted to put mempty 
instance Monoid Bool where
  mempty = False


data TM = TM {start :: Char, diag :: Int, states :: Map Char State} deriving (Show)
data Action = Action {write :: Bool, moveRight :: Bool, nextState :: Char } deriving (Show)
data State = State {sid :: Char, actions :: (Action,  Action)} deriving (Show)




parseTM :: [String] -> TM
parseTM ls = TM (head $ last $ words $ head ls) (read $ words (ls!!1)!!5) $ M.fromList $ zip ids sts
  where
    cs = chunksOf 10 $ drop 3 ls
    sts = parseState <$> cs
    ids = sid <$> sts


parseState :: [String] -> State
parseState ls = State (head $ reverseTake 2 $ head ls) (parseAction (drop 2 $ take 5 ls), parseAction (drop 6 ls) )


parseAction :: [String] -> Action
parseAction ls = Action (reverseTake 2 (head ls) == "1.")
                        (reverseTake 6 (ls!!1) == "right.")
                        (head $ words (ls!!2) !!4)


reverseTake :: Int -> [a] -> [a]
reverseTake n xs = reverse $ take n $ reverse xs


run :: TM -> (Char, Tape Bool)
run tm@(TM startState diagnostic states) = go diagnostic (startState, tape)
  where
    tape = Tape [] False []
    go :: Int -> (Char, Tape Bool) -> (Char, Tape Bool)
    go 0 tape = tape
    go n (stateName, tape@(Tape _ f _)) = go (n-1) (nx, if mv then moveR newTape else moveL newTape)
      where
        (a0, a1) = actions $ states M.! stateName
        (Action wt mv nx) = if f then a1 else a0
        newTape = update wt tape


day25 :: IO ()
day25 = do
  inLines <- getLines 25
  let tm :: TM
      tm = parseTM inLines
      sumT (Tape l f r) = f + sum l + sum r
     
  putStrLn $ "Day25: part1: " ++ show (sumT $ (\b -> if b then 1 else 0) <$> snd (run tm))

  return ()


test = [
  "Begin in state A."
  , "Perform a diagnostic checksum after 6 steps."
  , ""
  , "In state A:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state B."
  , "  If the current value is 1:"
  , "    - Write the value 0."
  , "    - Move one slot to the left."
  , "    - Continue with state B."
  , ""
  , "In state B:"
  , "  If the current value is 0:"
  , "    - Write the value 1."
  , "    - Move one slot to the left."
  , "    - Continue with state A."
  , "  If the current value is 1:"
  , "    - Write the value 1."
  , "    - Move one slot to the right."
  , "    - Continue with state A."
  ]