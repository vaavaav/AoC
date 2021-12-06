import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.HashMap.Strict as H (singleton, insertWith, foldr, (!?), HashMap)

simulate :: Int -> Int -> HashMap Int Int -> HashMap Int Int
simulate days d h
         | days < d  = h
         | otherwise = simulate days (d+1) $ maybe h (f h) (h !? d)  
   where f h n = foldl (g n) h [d+9,d+16..days]
         g n h d = insertWith (+) d n h 


init_ :: Int -> [Int] -> HashMap Int Int
init_ days x   = foldl g (singleton 0 $ length x) (concatMap f x)
   where f a   = [a+1,a+8..days] 
         g h a = insertWith (+) a 1 h

solve :: Int -> [Int] -> Int
solve days = H.foldr (+) 0 . simulate days 1 . init_ days

parse :: String -> [Int]
parse = map read . splitOn ","

main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs 
