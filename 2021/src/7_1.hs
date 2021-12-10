import Data.List
import System.Environment 
import Data.List.Split (splitOn)

fuel :: Int -> [Int] -> Int 
fuel x = foldr ((+) . abs . (x-)) 0

optimum ::  Int -> Int -> [Int] -> Int 
optimum min max l
  | min == max              = fuel min l 
  | fuel min l < fuel max l = optimum min mp l
  | otherwise               = optimum mp max l
  where mp = (min + max) `div` 2

solve :: [Int] -> Int
solve l = optimum (minimum l) (maximum l) l

parse :: String -> [Int]
parse = map read . splitOn ","

main = print . solve . parse =<< readFile . head =<< getArgs 
