import Data.List
import System.Environment 
import Data.HashMap.Strict (filterWithKey, findWithDefault, HashMap, elems, fromList, (!?), (!))

lowPoints :: HashMap (Int,Int) Int -> [Int]
lowPoints h = elems $ filterWithKey f h
 where f (l,c) n = all (> n) 
                 $ map (flip (findWithDefault 9) h)
                 $ [(l-1,c),(l+1,c),(l,c-1),(l,c+1)]

start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList $ zip [(l,c) | l <- [0..ls-1], c <- [0..cs-1]] $ concat x
 where (ls,cs) = (length x, length $ head x)

solve :: [[Int]] -> Int
solve = sum . map (+1) . lowPoints . start
        
parse :: String -> [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve .  parse =<< readFile =<< head <$> getArgs 
