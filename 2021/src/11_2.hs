import Data.List as L
import System.Environment 
import Data.HashMap.Strict as HM (insert, size, adjust, filter, map, keys, HashMap, elems, fromList)

flash :: HashMap (Int,Int) Int -> HashMap (Int,Int) Int
flash h 
      | null k9 = h
      | otherwise = flash (foldr f h k9)
  where k9 = keys $ HM.filter (> 9) h
        f (l,c) h = HM.insert (l,c) 0 
                    $ foldr (adjust (\v -> v + fromEnum (v > 0))) h 
                    $ delete (l,c) [(ls,cs) | ls <- [l-1..l+1], cs <- [c-1..c+1]]


simulate :: Int -> HashMap (Int,Int) Int -> Int
simulate n h
    | all (== 0) $ elems h = n
    | otherwise = simulate (n+1) h'
    where h' = flash $ HM.map (+1) h


start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList [((l,c),e') | (l,e) <- zip [0..] x, (c,e') <- zip [0..] e]

solve :: [[Int]] -> Int
solve = simulate 0 . start
        
parse :: String -> [[Int]]
parse = L.map (L.map (read . pure)) . lines

main = print . solve .  parse =<< readFile . head =<< getArgs  
