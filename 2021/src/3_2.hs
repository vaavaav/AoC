import Data.List
import System.Environment 

rating :: ([Int] -> Int) -> Int -> [[Int]] -> Int
rating _ _ [n] = sum $ zipWith (\i n -> n*2^i) [0..] $ reverse n
rating f n l = rating f (n+1) (filter (\l -> cd == (l !! n)) l)  
        where cd = f (transpose l !! n)

solve :: [[Int]] -> Int
solve l = (rating mcd 0 l) * (rating lcd 0 l)
        where mcd l = fromEnum $ 2 * sum l >= length l
              lcd l = 1 - mcd l

parse :: String ->  [[Int]]
parse = map (map (read . pure)) . lines

main = print . solve . parse =<< readFile . head =<< getArgs 
