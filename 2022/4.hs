import System.Environment
import Data.List
import Data.List.Split (splitOneOf)

-- Part 1 

solve1 = length . filter (\(a,b) -> (a `isInfixOf` b) || (b `isInfixOf` a)) 

-- Part 2

solve2 = length . filter (not . null . uncurry intersect) 

-- parse

parse :: String -> [([Int],[Int])]
parse = map (range . map read . splitOneOf "-,") . lines 
 where range [x,y,n,m] = ([x..y],[n..m])

-- 

main = print . (solve1 `split` solve2) . parse =<< readFile . head =<< getArgs

--- utils

(f `split` g) x = (f x, g x)