import Data.List
import System.Environment 


solve :: [[Int]] -> Int
solve = product
      . foldl (\[m,l] (i,n) -> [m + n*2^i, l + (1-n)*2^i]) [0,0]
      . zip [0..]
      . reverse 
      . map (\l -> fromEnum $ 2 * sum l >= length l)
      . transpose

parse :: String ->  [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve . parse =<< readFile =<< head <$> getArgs 
