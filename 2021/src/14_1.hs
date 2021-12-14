import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.HashMap.Strict (HashMap, fromListWith, elems)

subs :: [(String,Char)] -> String -> String
subs _ ""  = ""
subs _ [x] = [x] 
subs l (h:s:t) = f l [h,s] ++ subs l (s:t)
 where f [] [a,b] = [a]
       f ((h,o):t) x@[a,b] 
         | h == x = [a,o]
         | otherwise = f t x

step :: Int -> (String,[(String,Char)]) -> String
step n (s,l) = iterate (subs l) s !! n

solve :: Int -> (String, [(String,Char)]) -> Int
solve n = (\l -> maximum l - minimum l) 
        . elems
        . fromListWith (+) 
        . (`zip` repeat 1) 
        . step n

parse :: String -> (String, [(String,Char)])
parse = bimap concat f . splitAt 1 . lines
 where f = map g . tail
       g = (\[[a,b],[o]] -> ([a,b],o)) . splitOn " -> "  


main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs
