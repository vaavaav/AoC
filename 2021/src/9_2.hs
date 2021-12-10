import Data.List
import System.Environment 
import Data.HashMap.Strict (keys, filterWithKey, findWithDefault, HashMap, elems, fromList, (!?), (!))

getBasinSize :: HashMap (Int,Int) Int -> [(Int,Int)] -> (Int,Int) -> Int
getBasinSize h previous (l,c) = (1 +) 
                              $ sum
                              $ map (getBasinSize h ((l,c):cs))  
                              $ filter (\c -> let v = findWithDefault 9 c h in v > n && v < 9)
                              $ cs
  where n = h ! (l,c)
        cs = [(l-1,c),(l+1,c),(l,c-1),(l,c+1)] \\ previous

basins :: HashMap (Int,Int) Int -> [Int]
basins h = map (getBasinSize h []) $ keys $ filterWithKey f h
 where f (l,c) n = all (> n) 
                 $ map (flip (findWithDefault 9) h)
                 $ [(l-1,c),(l+1,c),(l,c-1),(l,c+1)]

start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList $ zip [(l,c) | l <- [0..ls-1], c <- [0..cs-1]] $ concat x
 where (ls,cs) = (length x, length $ head x)

-- solve :: [[Int]] -> Int
solve = basins . start
        
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve .  parse =<< readFile =<< head <$> getArgs 
