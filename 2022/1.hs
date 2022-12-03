import Data.List
import System.Environment

-- Part 1

solve1 = maximum . map sum

-- Part 2

solve2 = sum . take 3 . reverse . sort . map sum

-- Parse

parse :: String -> [[Int]]
parse = map (map read)
      . filter (/= [""])
      . groupBy (\x y -> null x == null y)
      . lines  

---

main = print . (solve1 `split` solve2) . parse =<< readFile . head =<< getArgs

---

(f `split` g) x = (f x, g x)



