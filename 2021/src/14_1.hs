import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.HashMap.Strict as H (HashMap, member, fromListWith, elems, union, singleton, foldrWithKey, (!), adjust, insertWith, delete, toList)

type Dict = HashMap Char (HashMap Char Int)

subs :: [((Char,Char),Char)] -> Dict -> Dict
subs l h = fst $ mapAccumL (f l) h (toList h)
 where f [] h' e = (h', e) 
       f (((a,b),c):t) h' e@(k,v)
         | a == k && b `H.member` v = (g a c b (v ! b) h', (k,v))
         | otherwise = f t h' e 
       g a c b n = insertWith H.union c (singleton b n)
                 . adjust (insertWith (+) c n . H.delete b) a

step :: Int -> Dict -> [((Char,Char),Char)] -> Dict
step n h l = iterate (subs l) h !! n

start :: String -> Dict
start l@(_:t) = fromListWith H.union $ zip l $ map (`singleton` 1) (t ++ "\0")

solve n = --(\l -> maximum l - minimum l)
        -- . map length
         elems
        . uncurry (step n) 
        . first start

parse :: String -> (String, [((Char,Char),Char)])
parse = bimap concat f . splitAt 1 . lines
 where f = map g . tail
       g = (\[[a,b],[o]] -> ((a,b),o)) . splitOn " -> "  

main = print =<< (\(n:f:t) -> solve (read n) . parse <$> readFile f) =<< getArgs
