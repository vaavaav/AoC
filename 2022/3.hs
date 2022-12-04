import System.Environment
import Data.List
import Data.Maybe

priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

priority :: Char -> Int
priority c = fromJust (c `elemIndex` priorities) + 1

-- Part 1 

solve1 = sum . map priority 
       . concatMap (nub . intersect' . f) 
      where f s = splitAt ((length s) `div` 2) s 
            intersect' = uncurry intersect

-- Part 2

solve2 = sum . map priority 
       . concatMap (nub . foldl1 intersect) 
       . unfoldr (\l -> if null l then Nothing else Just $ splitAt 3 l)

-- 

main = print . (solve1 `split` solve2) . lines =<< readFile . head =<< getArgs

--- utils

(f `split` g) x = (f x, g x)



