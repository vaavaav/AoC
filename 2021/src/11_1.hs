import Data.List as L
import System.Environment 
import Data.HashMap.Strict as HM (insert, size, adjust, filter, map, keys, HashMap, elems, fromList)
import Data.Bool

flash :: HashMap (Int,Int) Int -> HashMap (Int,Int) Int
flash h 
      | null k9 = h
      | otherwise = flash (foldr f h k9)
  where k9 = keys $ HM.filter (> 9) h
        f (l,c) h = HM.insert (l,c) 0 
                    $ foldr (adjust (\v -> v + fromEnum (v > 0))) h 
                    $ delete (l,c) [(ls,cs) | ls <- [l-1..l+1], cs <- [c-1..c+1]]


simulate :: Int -> HashMap (Int,Int) Int -> Int
simulate 0 _ = 0 
simulate n h = size (HM.filter (== 0) h') + simulate (n-1) h' 
    where h' = flash $ HM.map (+1) h


start :: [[Int]] -> HashMap (Int,Int) Int 
start x = fromList [((l,c),e') | (l,e) <- zip [0..] x, (c,e') <- zip [0..] e]

solve :: Int -> [[Int]] -> Int
solve n = simulate n . start
        
parse :: String -> [[Int]]
parse = L.map (L.map (read . pure)) . lines


main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs 
