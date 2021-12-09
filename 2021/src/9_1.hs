import Data.List
import System.Environment 
import Data.HashMap.Strict (HashMap, size, fromList, (!?), (!), keys)
import Data.Maybe
import Data.Bool


lowPoints :: HashMap (Int,Int) Int -> [Int]
lowPoints h = foldr isLowPoint [] (keys h)
 where isLowPoint (l,c) p = bool p ((h ! (l,c)):p) 
                          $ all (> (h ! (l,c)))
                          $ catMaybes
                          $ [h !? (l-1,c), h !? (l+1,c), h !? (l,c-1), h !? (l,c+1)] 

solve :: [[Int]] -> Int
solve x = sum
        $ map (+1)
        $ lowPoints
        $ fromList 
        $ zip [(l,c) | l <- [0..ls-1], c <- [0..cs-1]]
        $ concat x
 where (ls,cs) = (length x, length $ head x)
        
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve .  parse =<< readFile =<< head <$> getArgs 
