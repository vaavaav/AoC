import Data.List
import System.Environment 
import Data.List.Split (splitOn, chunksOf)
import Data.Bifunctor
import Data.HashMap.Strict (HashMap, fromListWith, toList, empty)

subs :: [(String,Char)] -> (HashMap String Int,HashMap Char Int) -> (HashMap String Int,HashMap Char Int)
subs l (s,c) = fromListWith (+) $ concatMap (f l) $ toList s
  where f [] x = [x]
        f ((h,o):t) x@(k@[a,b],v)
          | h == k = [([a,o],v),([o,b],v)]
          | otherwise = f t x  

step :: Int -> (HashMap String Int, HashMap Char Int) -> [(String,Char)] -> (HashMap String Int, HashMap Char Int)
step n (s,c) l = iterate (subs l) (h,c) !! n

start :: String -> (HashMap String Int, HashMap Char Int)
start l@(h:t) = (s,c)
  where s = fromListWith (+) $ zipWith (\a b -> ([a,b],1)) l t
        c = fromListWith (+) $ (`zip` repeat 1) l

--solve :: Int -> (String, [(String,Char)]) -> Int
solve n (s,l) = -- (\l -> maximum l - minimum l)
              -- $ toList
              snd
              $ step n (start s)


parse :: String -> (String, [(String,Char)])
parse = bimap concat f . splitAt 1 . lines
 where f = map g . tail
       g = (\[[a,b],[o]] -> ([a,b],o)) . splitOn " -> "  

main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs
