
module Day6 where


--import Data.List
import Data.Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Utils
import GHC.IO.Handle.Types (checkHandleInvariants)


redistribute :: Vector Int -> Vector Int
redistribute ns = V.zipWith (+) rinc $ ns V.// [(ix, 0)]
  where
    mx = maximum ns
    ix = fromMaybe (error "Shouldn't get here") $ V.elemIndex mx ns
    ln = length ns
    (q, r) = quotRem mx ln
    inc = V.replicate r (q+1) V.++ V.replicate (ln-r) q
    rinc = V.drop (ln-ix-1) inc V.++ V.take (ln-ix-1) inc


hash :: Vector Int -> String 
hash ns = concat $ show <$> ns


checkHash :: Int -> Vector Int -> Map String Int -> (Maybe Int, Map String Int)
checkHash steps ns hs = if h `member` hs then (Just $ hs ! h, hs) else (Nothing, insert h steps hs)
  where
    h = hash ns


loop :: Int -> Vector Int -> Map String Int -> (Int, Int)
loop n bs inh = case checkHash n bs inh of
                   (Just s, outh) -> (s, n)
                   (Nothing, outh) -> loop (n+1) (redistribute bs) outh


day6 :: IO ()
day6 = do
  inLines <- getLines 6
  let bs :: Vector Int
      bs = V.fromList $ read <$> words (head inLines)
      --bs = V.fromList [0, 2, 7, 0]
      (fst, snd) = loop 0 bs empty
     
  putStrLn $ "Day6: part1: " ++ show snd
  putStrLn $ "Day6: part2: " ++ show (snd - fst)

  return ()


