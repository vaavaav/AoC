import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.HashMap.Strict as H (HashMap, fromListWith, elems, unionWith, singleton, insertWith, toList, adjust, (!?), delete)

type Dict = HashMap Char (HashMap Char Integer)

subs :: [((Char,Char),Char)] -> Dict -> Dict
subs [] h = h
subs (((a,b),c):t) h = maybe id f ((!? b) =<< (h !? a)) 
                     $ subs t (adjust (H.delete b) a h)
 where f n = insertWith (unionWith (+)) a (singleton c n)
           . insertWith (unionWith (+)) c (singleton b n)

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
