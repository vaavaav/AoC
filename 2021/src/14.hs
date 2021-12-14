import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.HashMap.Strict as H (HashMap, fromListWith, elems, unionWith, singleton, (!), insertWith, toList)

type Dict = HashMap Char (HashMap Char Integer)

subs :: [((Char,Char),Char)] -> Dict -> Dict
subs l = fromListWith (H.unionWith (+))
       . map (second (uncurry singleton)) 
       . concatMap (\(k,v) -> concatMap (g l k) (toList v)) 
       . toList
 where g [] k x = [(k,x)]
       g (((a,b),c):t) k (v,n)
         | a == k && b == v = [(k,(c,n)),(c,(v,n))]
         | otherwise = g t k (v,n)

step :: Int -> Dict -> [((Char,Char),Char)] -> Dict
step n h l = iterate (subs l) h !! n

start :: String -> Dict
start l@(_:t) = fromListWith (H.unionWith (+)) $ zip l $ map (`singleton` 1) (t ++ "\0")

solve n = (\l -> maximum l - minimum l)
        . map (sum . elems)
        . elems
        . uncurry (step n) 
        . first start

parse :: String -> (String, [((Char,Char),Char)])
parse = bimap concat (map f . tail) . splitAt 1 . lines
 where f = (\[[a,b],[o]] -> ((a,b),o)) . splitOn " -> "  

main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs
