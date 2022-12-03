import System.Environment
import Data.List
import Data.Maybe

priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

priority :: Char -> Int
priority c = fromJust (c `elemIndex` priorities) + 1

-- Part 1 

solve1 = sum 
       . map priority 
       . concatMap (nub . intersect' . f) 
      where f s = splitAt ((length s) `div` 2) s 
            intersect' = uncurry intersect

-- Part 2

triple :: [a] -> [[a]]
triple [] = []
triple (x:y:z:t) = [x,y,z] : triple t 

solve2 = sum 
       . map priority 
       . concatMap (nub . foldl1 intersect) 
       . triple  

-- 

main = print . (solve1 `split` solve2) . lines =<< readFile . head =<< getArgs

---

(f `split` g) x = (f x, g x)



