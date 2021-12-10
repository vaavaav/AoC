import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.HashMap.Strict as H (insertWith, size, filter, empty, HashMap)

toHashmap :: [(Int,Int)] -> HashMap (Int,Int) Int -> HashMap (Int,Int) Int
toHashmap = flip $ foldr (\i h -> insertWith (+) i 1 h)

makeLine :: [[Int]] -> [(Int,Int)]
makeLine [[x1,y1],[x2,y2]] 
     | x1 == x2  = zip (repeat x1) [y1,y'..y2]
     | y1 == y2  = zip [x1,x'..x2] (repeat y1)
     | otherwise = zip [x1,x'..x2] [y1,y'..y2]
    where x' = x1 + signum (x2-x1)
          y' = y1 + signum (y2-y1)

solve :: [[[Int]]] -> Int
solve = size . H.filter (>1) . foldr (toHashmap . makeLine) H.empty

parse :: String -> [[[Int]]]
parse = map (map (map read . splitOn "," ) . splitOn " -> ") . lines

main = print . solve . parse =<< readFile . head =<< getArgs 
