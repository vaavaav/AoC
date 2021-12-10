import Data.List
import System.Environment

solve = length
      . filter (<0)
      . (\l@(_:t) -> zipWith (-) l t)
      . (\l@(_:h:t) -> zipWith3 (\a b c -> a+b+c) l (h:t) t)
     
parse = map read . lines

main = print . solve . parse =<< readFile . head =<< getArgs
