import Data.List
import System.Environment 
import Data.Char
import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap, fromListWith, (!))

start :: [[String]] -> HashMap String [String] 
start x = fromListWith (++) [e | [a,b] <- x, e <- [(a,[b]),(b,[a])]]

route :: HashMap String [String] -> [String] -> String -> [[String]]
route h visited "end" = ["end":visited] 
route h visited  now  = concatMap (route h (now:visited))
                      $ (h ! now) \\ filter (any isLower) visited

solve :: [[String]] -> Int
solve input = length $ route (start input) [] "start" 
        
parse :: String -> [[String]]
parse = map (splitOn "-") . lines

main = print . solve . parse =<< readFile . head =<< getArgs  
