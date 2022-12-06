import System.Environment
import Data.List

subs :: (Int, a) -> [a] -> [a]
subs (i,x) l = let (before,(_:after)) = splitAt (i-1) l in before ++ [x] ++ after

move :: (String -> String) -> [String] -> (Int,Int,Int) -> [String]
move sort cargo (n,from,to) = subs (from, left) $ subs (to, to') cargo
  where (transferred, left) = splitAt n (cargo !! (from - 1))
        to' = (sort transferred) ++ (cargo !! (to - 1))
---

solve1 = map head . uncurry (foldl (($) . move reverse))

solve2 = map head . uncurry (foldl (($) . move id))

-- parse

parse = parseCargo `split` parseMoves

parseCargo = words
           . filter (`elem` (' ':['A'..'Z']))
           . concat
           . transpose 
           . takeWhile (']' `elem`) 
           . lines

parseMoves = map ((\[_,n,_,x,_,y] -> (read n,read x,read y)) . words)
           . drop 2
           . dropWhile (']' `elem`) 
           . lines

-- 

main = print . (solve1 `split` solve2) . parse =<< readFile . head =<< getArgs

--- utils

(f `split` g) x = (f x, g x)