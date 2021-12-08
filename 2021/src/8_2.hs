import Data.List
import System.Environment 
import Data.List.Split (splitOn)
import Data.Maybe

decode :: [String] -> [String] -> Int
decode ns = foldl (\acc x -> (acc * 10) + x) 0 
          . catMaybes 
          . map (`elemIndex` ns)

toConfig :: [String] -> [String]
toConfig [one,seven,four,a,b,c,d,e,f,eight] 
 = map sort [zero, one, two, three, four, five, six, seven, eight, nine]
 where [six,e',f']   = sortOn (length . intersect one)  [d,e,f] 
       [a',b',three] = sortOn (length . intersect one)  [a,b,c]
       [two, five]   = sortOn (length . intersect six)  [a',b']
       [zero, nine]  = sortOn (length . intersect five) [e',f'] 

solve :: [[[String]]] -> Int
solve = foldr (\[i,o] a -> a + decode (toConfig $ sortOn length i) (map sort o)) 0
        
parse :: String -> [[[String]]]
parse = map (map words . splitOn " | ") . lines
main = print . solve . parse =<< readFile =<< head <$> getArgs 
