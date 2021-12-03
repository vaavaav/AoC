import Data.List
import System.Environment 

solve :: [[Int]] -> Int
solve = product
      . map sum
      . transpose
      . zipWith (\i l -> [2^i * mcb l, 2^i * lcb l]) [0..]
      . reverse 
      . transpose
      where mcb l = fromEnum $ 2 * sum l >= length l
            lcb l = 1 - mcb l
            

parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve . parse =<< readFile =<< head <$> getArgs 
