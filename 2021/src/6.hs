import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.HashMap.Strict as H (fromListWith, insertWith, (!?), HashMap, foldl)
import Data.Maybe

simulate :: Int -> Int -> HashMap Int Int -> Int
simulate d days h 
         | d > days  = 0
         | f == 0    = simulate (d+1) days h
         | otherwise = f + simulate (d+1) days h' 
   where h' = foldr (flip (insertWith (+)) f) h [d+9,d+16..days]
         f = fromMaybe 0 (h !? d)     

solve :: Int -> [Int] -> Int
solve days = simulate (-8) days 
           . fromListWith (+)
           . (\l -> [(f-8,1) | f <- l])

parse :: String -> [Int]
parse = map read . splitOn ","

main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs 
