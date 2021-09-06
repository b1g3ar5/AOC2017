{-# LANGUAGE BangPatterns #-}

module Day17 where

import Data.Sequence hiding (take)
import Data.List

import Data.Bool
import Utils

import System.TimeIt

{- 

The list has a size and a position. You have to do this:

  - move to position+steps `mod` size
  - insert a number (and increment the size, and increment the position
    of all higher positions)
  - move forward 1 to that number

So, what type to use?

List no good because insertion too difficult
Map no good because of the address increment

BUT!!! For part 2 we don't need to keep the list!!!

-}

-- A cyclic sequence with a focus and length
data Buffer a = Buffer {seq :: Seq a, focus :: Int, sz :: Int} deriving (Show)


initialBuffer :: Buffer Int
initialBuffer = Buffer (singleton 0) 0 1


makeBuffer :: Int -> Int -> Buffer Int
makeBuffer step sz = foldl insert initialBuffer [1..sz]
  where
    insert (Buffer s f n) x = Buffer (insertAt ix x s) ix (n+1)
      where
        !ix = 1 + (f + step) `mod` n 


day17 :: IO ()
day17 = do

  let step = 363    
      (Buffer s1 f1 _) = makeBuffer step 2017
      (Buffer s2 _ _) = makeBuffer step 50000000
  putStrLn $ "Day17: part1: " ++ show (s1 `index` (f1+1))
  putStrLn $ "day17b: " ++ show (day17b step)
  putStrLn $ "Day17: part2: " ++ show (part2 step)

  return ()


day17b :: Int -> Int
day17b step = insert 0 1 0 where
    insert focus sz next
      | newSz > 50000000 = next
      | otherwise = insert newFocus newSz $! if newFocus == 1 then sz + q else next 
        where
          q = (sz - focus) `div` (step + 1)
          newFocus = (focus + step) `mod` sz + 1
          newSz = sz + 1


cursors :: Int -> [Int]
cursors jump = scanl' nextCursor 0 [1..]
  where
    nextCursor cursor size = (cursor+jump)`rem`size + 1
{-# Inline cursors #-} -- helps list fusion!

-- | Special case for when we only need to know what number is going
-- to follow the zero. Because the 0 is always going to be at the zero
-- index, whatever the last element to be written to the 1 index must
-- be the element that directly follows the zero.
part2 :: Int ->Int
part2 = last . elemIndices 1 . take 50000000 . cursors