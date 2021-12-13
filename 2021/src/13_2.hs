import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.Bool
import Data.HashSet (size , fromList , member)

fold :: Int -> Int -> Int
fold n n1
     | n1 < n = n1
     | otherwise = 2*n - n1

folds :: [Either Int Int] -> (Int,Int) -> (Int,Int)
folds [] p = p
folds (Left  x:t) (x1,y1) = folds t (fold x x1,y1)
folds (Right y:t) (x1,y1) = folds t (x1,fold y y1)

toString :: [(Int,Int)] -> String
toString x = unlines [[if (c,l) `member` h then '#' else '.' | c <- [0..cs]]  | l <- [0..ls]] 
    where h = fromList x
          (cs,ls) = bimap maximum maximum $ unzip x

solve :: ([(Int,Int)], [Either Int Int]) -> String
solve (p,f) = toString $ map (folds f) p
        
parse :: String -> ([(Int,Int)], [Either Int Int])
parse = bimap f g . span (',' `elem`) . lines
    where f = map (bimap read (read . tail) . break (== ','))
          g = map (\l -> bool Right Left ('x' `elem` l) $ h l) . tail
          h = read . last . splitOn "="
          
main = putStrLn . solve . parse =<< readFile . head =<< getArgs  
