import Data.List
import System.Environment

solve = product 
      . map sum 
      . transpose

parse = map ((\[s,n] -> f (head s, read n)) . words) 
      . lines
      where f ('f', n) = [n, 0]
            f ('u', n) = [0,-n]
            f ('d', n) = [0, n]

main = print . solve .  parse =<< readFile =<< head <$> getArgs 
