import System.Environment
import Data.List


move :: (String -> String) -> Int -> Int -> Int -> [String] -> [String]
move order n from to cargo = zipWith f [1..] cargo 
    where f i e
            | i == from = from' 
            | i == to = (order transferred) ++ e
            | otherwise = e 
          (transferred,from') = splitAt n (cargo !! (from - 1))

---

solve :: ([String], [[String] -> [String]]) -> String
solve = map head . uncurry (foldl (flip ($)))

-- parse

parse1 = parseCargo `split` (parseMoves $ move reverse) 

parse2 = parseCargo `split` (parseMoves $ move id) 

parseCargo = words
           . filter (`elem` (' ':['A'..'Z']))
           . concat
           . transpose 
           . takeWhile (']' `elem`) 
           . lines

parseMoves f = map ((\[_,n,_,x,_,y] -> f (read n) (read x) (read y)) . words)
           . drop 2
           . dropWhile (']' `elem`) 
           . lines

-- 

main = print . ((solve . parse1) `split` (solve . parse2)) =<< readFile . head =<< getArgs

--- utils

(f `split` g) x = (f x, g x)