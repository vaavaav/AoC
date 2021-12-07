import Data.List
import System.Environment

solve = length 
      . filter (<0)
      . (\l -> zipWith (-) l (tail l))

parse = map read . lines

main = print . solve . parse =<< readFile =<< head <$> getArgs
