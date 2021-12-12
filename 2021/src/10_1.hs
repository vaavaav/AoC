import Data.List
import System.Environment 
import Data.Either

points :: Char -> Int 
points ')' = 3 
points ']' = 57 
points '}' = 1197 
points '>' = 25137 

isOpen :: Char -> Bool
isOpen = (`elem` ['(','[','{','<'])

closePair :: Char -> Char
closePair '(' = ')' 
closePair '[' = ']' 
closePair '{' = '}' 
closePair '<' = '>' 

getCorruption :: [Char] -> Either [Char] Char
getCorruption [_] = Left []
getCorruption (o:c:t)
    | isOpen c = either (getCorruption . (o:)) Right (getCorruption (c:t))
    | closePair o == c = Left t
    | otherwise = Right c


solve :: [[Char]] -> Int
solve = foldr ((+) . points) 0
      . rights
      . map getCorruption

parse :: String -> [[Char]]
parse = lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
