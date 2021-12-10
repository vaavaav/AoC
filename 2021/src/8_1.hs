import Data.List
import System.Environment 
import Data.List.Split (splitOn)

solve :: [String] -> Int
solve = length . filter ((`elem` [2,3,4,7]) . length) 
        
parse :: String -> [String]
parse = concatMap (words . last . splitOn " | ") . lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
