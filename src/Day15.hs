{-# LANGUAGE BangPatterns #-}

module Day15 where

import System.TimeIt
import Data.Bits ( Bits((.&.)) )
import Utils
import Data.Array.Base (accum)
import qualified Data.Stream as S


seedA, seedB, factorA, factorB, prod :: Int
seedA = 277 
factorA = 16807
seedB = 349
factorB = 48271
prod = 2147483647


genA1, genB1 :: Int -> Int
genA1 x = x * factorA `mod` prod
genB1 x = x * factorB `mod` prod

genA2, genB2 :: Int -> Int
genA2 x = if y `mod` 4 == 0 then y else genA2 y
  where
    y = x * factorA `mod` prod
genB2 x = if y `mod` 8 == 0 then y else genB2 y
  where
    y = x * factorB `mod` prod


judge :: Int -> Int -> Bool
judge x y = (x .&. 0xffff) == (y .&. 0xffff)


streamA1, streamB1, streamA2, streamB2 :: S.Stream Int
streamA1 =  S.iterate genA1 seedA
streamB1 =  S.iterate genB1 seedB
streamA2 =  S.iterate genA2 seedA
streamB2 =  S.iterate genB2 seedB

streamAB1, streamAB2 :: S.Stream Bool
streamAB1 = S.zipWith judge streamA1 streamB1
streamAB2 = S.zipWith judge streamA2 streamB2


day15 :: IO ()
day15 = do
  putStrLn $ "day15a: part1: " ++ show (length $ filter id $ S.take 40000000 streamAB1)
  putStrLn $ "day15a: part1: " ++ show (length $ filter id $ S.take 5000000 streamAB2)

  return ()


