import Data.List
import System.Environment 
import Data.Bifunctor
import Data.Maybe

points :: Char -> Int 
points ')' = 1
points ']' = 2 
points '}' = 3 
points '>' = 4 

isOpen :: Char -> Bool
isOpen = (`elem` ['(','[','{','<'])

closePair :: Char -> Char
closePair '(' = ')' 
closePair '[' = ']' 
closePair '{' = '}' 
closePair '<' = '>' 

pairing :: [Char] -> Maybe ([Char],[Char])
pairing [o] = Just ([],[closePair o]) 
pairing (o:c:t)
    | not (isOpen o) = Nothing
    | isOpen c = f =<< pairing (c:t)
    | closePair o == c = Just (t,[])
    | otherwise = Nothing
  where f ([],l) = Just ([],l ++ [closePair o])
        f (r,l) = second (l++) <$> pairing (o:r) 
  
complete :: [Char] -> Maybe [Char]
complete [] = Just []
complete l = f =<< pairing l
  where f ([],l) = Just l
        f (r,[]) = complete r


solve :: [[Char]] -> Int
solve = (\l -> l !! (length l `div` 2))
    . sort
    . map (foldl f 0)
    . mapMaybe complete
  where f a c = points c + a * 5 
    
parse :: String -> [[Char]]
parse = lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
