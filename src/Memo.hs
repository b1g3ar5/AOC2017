{-# LANGUAGE BangPatterns #-}

module Memo (
  memoise
  --, memo
  --, memoFix
  , Tree(..)
  , index
  , nats
  , rindex
  , fromInt
  , pad
  , toInt
  , finiteNats
  , memoMain
) where

--import Data.MemoTrie
import Data.Function (fix)
import qualified Data.Map as M


-- See https://stackoverflow.com/questions/3208258/memoization-in-haskell

{-
Simple memoisation

The nats are all put in a clever tree and then other things can be put in the tree
by mapping over the nats (you just need to code and decode to/from a nat).
-}

table :: M.Map i a
table = M.empty

apply :: Ord i => M.Map i a -> i -> a
apply = (M.!)

tablulate :: Ord i =>  i -> a -> M.Map i a
tablulate k v = M.insert k v table

lmemo :: Ord i => (i -> a) -> i -> a
lmemo f k = apply (tablulate k $ f k) k

--memoize :: (Int -> a) -> (Int -> a)
--memoize f = (map f [0 ..] !!)



memoMain :: IO ()
memoMain = do
  putStrLn $ "fix fTest 123: " ++ show (fix testF 123)
  putStrLn $ "fastestF 123801: " ++ show (fastestF 123801)
  putStrLn $ "fastestF 12380192300: " ++ show (fastestF 12380192300)
  putStrLn $ "fastestF 12793129379123: " ++ show (fastestF 12793129379123)
  putStrLn $ "fastestF 1230891823091823018203123: " ++ show (fastestF 1230891823091823018203123)
  putStrLn $ "fastestF 12308918230918230182031231231293810923: " ++ show (fastestF 12308918230918230182031231231293810923)

  putStrLn $ "fastestKeys 123801: " ++ show (fastestKeys 123801)
  putStrLn $ "fastestKeys 12380192300: " ++ show (fastestKeys 12380192300)
  putStrLn $ "fastestKeys 12793129379123: " ++ show (fastestKeys 12793129379123)

  putStrLn $ "memoedKeyToDist 123801: " ++ show (memoedKeyToDist 123801)
  putStrLn $ "memoedKeyToDist 12380192300: " ++ show (memoedKeyToDist 12380192300)
  putStrLn $ "memoedKeyToDist 12793129379123: " ++ show (memoedKeyToDist 12793129379123)

  putStrLn $ "memoedTestF 123801: " ++ show (memoedTestF 123801)
  putStrLn $ "memoedTestF 12380192300: " ++ show (memoedTestF 12380192300)
  --timeIt $ putStrLn $ "memoedTestF 12793129379123: " ++ show (memoedTestF 12793129379123)

  --putStrLn $ "memoFix testF 123801: " ++ show (memoFix testF 123801)
  --putStrLn $ "memoFix testF 12380192300: " ++ show (memoFix testF 12380192300)
  --timeIt $ putStrLn $ "memoFix testF 12793129379123: " ++ show (memoFix testF 12793129379123)


recursiveF :: Integer -> Integer
recursiveF 0 = 0
recursiveF n = max n $ recursiveF (n `div` 2) +
                       recursiveF (n `div` 3) +
                       recursiveF (n `div` 4)


testF :: (Integer -> Integer) -> Integer -> Integer
testF _ 0 = 0
testF mf n = max n $ mf (n `div` 2) +
                     mf (n `div` 3) +
                     mf (n `div` 4)


memoedTestF = memoise testF id id id id



memoise :: ((a -> b) -> a -> b) 
          -> (a -> Integer) -> (Integer -> a) 
          -> (b -> Integer) -> (Integer -> b) 
          -> (Integer -> Integer)
memoise funcToMemoise fromA toA fromB toB = fastestFunc
  where
    transformedFunc :: (Integer -> Integer) -> Integer -> Integer    
    transformedFunc fii i = fromB $ funcToMemoise (toB . fii . fromA) (toA i)

    tree = fmap (transformedFunc fastestFunc) nats
    fastestFunc = index tree


-- The list of bools could be the key list and the int the distance
-- We calculate the distance using testF
keysToDist :: ([Bool] -> Int) -> [Bool] -> Int
keysToDist mg bs = fromIntegral $ testF mf i
  where
    mf = fromIntegral . mg . fromInt . fromIntegral
    i = toInt bs


memoedKeyToDist :: Integer -> Integer
memoedKeyToDist = memoise keysToDist toInt fromInt fromIntegral fromIntegral


-- To memoise testG we need to turn it into a testF!
transformedKeysToDist :: (Integer -> Integer) -> Integer -> Integer
transformedKeysToDist mf n = fromIntegral $ keysToDist mg bs
  where
    bs = fromInt n
    mg = fromIntegral . mf . toInt


keysTree :: Tree Integer
keysTree = fmap (transformedKeysToDist fastestKeys) nats


fastestKeys :: Integer -> Integer
fastestKeys = index keysTree



data Tree a = Tree (Tree a) a (Tree a)


instance Functor Tree where
    fmap f (Tree l m r) = Tree (fmap f l) (f m) (fmap f r)


data Tree2 a = One a | Two a a | Tree2 (Tree2 a) a (Tree2 a) deriving (Eq, Show)


getTree :: Tree a -> a
getTree (Tree _ x _) = x

instance Functor Tree2 where
  fmap f (One m) = One (f m)
  fmap f (Two m n) = Two (f m) (f n)
  fmap f (Tree2 l m r) = Tree2 (fmap f l) (f m) (fmap f r)


index :: Tree a -> Integer -> a
index (Tree _ m _) 0 = m
index (Tree l _ r) n = case (n - 1) `divMod` 2 of
                         (q,0) -> index l q
                         (q,1) -> index r q
                         _ -> error "This shouldn't happen in index"


index2 :: Tree2 a -> Integer -> a
index2 (One x) _ = x
index2 (Two x y) n = case (n - 1) `divMod` 2 of
                       (q,0) -> y
                       (q,1) -> x
                       _ -> error "This shouldn't happen in index"
index2 (Tree2 l _ r) n = case (n - 1) `divMod` 2 of
                           (q,0) -> index2 l q
                           (q,1) -> index2 r q
                           _ -> error "This shouldn't happen in index"


nats :: Tree Integer
nats = go 0 1
  where
    go !n !s = Tree (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2


finiteNats :: Integer -> Tree2 Integer
finiteNats limit = go 0 1
  where
    go !n !s
      | l > limit = One n
      | r > limit = Two n l
      | otherwise = Tree2 (go l s') n (go r s')
        where
          l = n + s
          r = l + s
          s' = s * 2
 

fTree :: Tree Integer
fTree = fmap (testF fastestF) nats


fastestF :: Integer -> Integer
fastestF = index fTree


-- The bool list here needs to be not padded!
-- 0 == [], 1 = True, 2 = False
rindex :: Tree a -> [Bool] -> a
rindex t bs = index t $ toInt bs


fromInt :: Integer -> [Bool]
fromInt 0 = [False]
fromInt i = helper i
  where
    helper 0 = []
    helper i = let (q,r) = i `divMod` 2 in (r==1) : helper q


pad :: Int -> a -> [a] -> [a]
pad n b bs = replicate (n - length bs) b ++ take n bs


toInt :: [Bool] -> Integer
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


-- ############# Example #############
fib0 :: Integer -> Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n-1) + fib0 (n-2)

-- Remove the recursion
fib1 :: (Integer -> Integer) -> Integer -> Integer
fib1 f 0 = 0
fib1 f 1 = 1
fib1 f n = f (n-1) + f (n-2)

-- Build a tree
fibTree :: Tree Integer
fibTree = fmap (fib1 fastestFib) nats

-- Get the answer from the tree
fastestFib :: Integer -> Integer
fastestFib = index fibTree



