import System.Environment
import Data.List

allDiff l = l == (nub l) 

findStart n l 
  | allDiff (take n l) = n 
  | otherwise = 1 + findStart n (tail l)

solve1 = findStart 4 

solve2 = findStart 14 

-- 

main = print . (solve1 `split` solve2) =<< readFile . head =<< getArgs

--- utils

(f `split` g) x = (f x, g x)