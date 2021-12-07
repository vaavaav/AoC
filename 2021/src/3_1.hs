import Data.List
import System.Environment 
import Data.Char

solve :: [[Int]] -> Int
solve = product
      . snd
      . foldr (\l (i,[e,g]) -> (i+1, [e + mcb l i, g + lcb l i])) (0,[0,0])
      . transpose 
     where mcb l i = (2*sum l `div` length l)*2^i
           lcb l i = (length l `div` (2 * sum l))*2^i      

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

main = print . solve . parse =<< readFile =<< head <$> getArgs 
