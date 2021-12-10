import Data.List
import System.Environment 

close :: Char -> Char
close '[' = ']'
close '(' = ')'
close '{' = '}'
close '<' = '>'

getCorrupted = map (\l@(h:_) -> span (/= close h) l)

solve = getCorrupted  

parse = lines

main = print . solve .  parse =<< readFile . head =<< getArgs 
