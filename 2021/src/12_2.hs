import Data.List
import System.Environment 
import Data.Char
import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap, fromListWith, (!))

start :: [[String]] -> HashMap String [String] 
start x = fromListWith (++) [e | [a,b] <- x, e <- [(a,[b]),(b,[a])]]

route :: HashMap String [String] -> [String] -> Bool -> String -> [[String]]
route _ visited _ "end" = ["end":visited] 
route h visited twice now = routeNormal ++ if twice then [] else routeTwice
    where canBeTwice  = filter (\l -> l `elem` visited && all isLower l) (h ! now) \\ ["start", "end"]
          routeTwice  = concatMap (route h (now:visited) True) canBeTwice
          routeNormal = concatMap (route h (now:visited) twice) $ (h ! now) \\ filter (all isLower) visited

solve :: [[String]] -> Int
solve input = length $ route (start input) [] False "start" 
        
parse :: String -> [[String]]
parse = map (splitOn "-") . lines

main = print . solve . parse =<< readFile . head =<< getArgs  
