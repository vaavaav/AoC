import Data.List
import System.Environment 
import Data.HashMap.Strict (keys, filterWithKey, findWithDefault, HashMap, fromList, (!?), (!))

getBasinSize :: HashMap (Int,Int) Int -> [(Int,Int)] -> [(Int,Int)] -> Int
getBasinSize _ _ [] = 0
getBasinSize h previous ((l,c):t)
             | (l,c) `elem` previous = 1 + getBasinSize h previous t
             | otherwise = (1 +)
                         $ getBasinSize h ((l,c):previous)
                         $ (++) t
                         $ filter (\c -> findWithDefault 9 c h < 9)
                         $ [(l-1,c),(l+1,c),(l,c-1),(l,c+1)] \\ (t ++ previous)

basins :: HashMap (Int,Int) Int -> [Int]
basins h = map (getBasinSize h [] . pure) $ keys $ filterWithKey f h
 where f (l,c) n = all ((> n) . flip (findWithDefault 9) h) [(l-1,c),(l+1,c),(l,c-1),(l,c+1)]

start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList $ zip [(l,c) | l <- [0..ls-1], c <- [0..cs-1]] $ concat x
 where (ls,cs) = (length x, length $ head x)

-- solve :: [[Int]] -> Int
solve = product . take 3 . reverse . sort . basins . start
        
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
