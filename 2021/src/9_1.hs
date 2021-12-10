import Data.List
import System.Environment 
import Data.HashMap.Strict (HashMap, filterWithKey, HashMap, elems, fromList)
import Data.Maybe

lowPoints :: HashMap (Int,Int) Int -> [Int]
lowPoints h = elems $ filterWithKey f h
 where f (l,c) n = all ((> n) . fromMaybe 9 . (h !?)) [(l-1,c),(l+1,c),(l,c-1),(l,c+1)]

start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList [((l,c),e') | (l,e) <- zip [0..] x, (c,e') <- zip [0..] e]

solve :: [[Int]] -> Int
solve = sum . map (+1) . lowPoints . start
        
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
