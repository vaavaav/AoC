import Data.List
import System.Environment


main = print
     . length
     . filter (< 0)
     . (\l@(_:t) -> zipWith (-)  l t)
     . (\l@(_:h:t) -> zipWith3 (\a b c -> a+b+c) l (h:t) t)
     . map (read :: String -> Int)
     . lines
     =<< readFile 
     =<< head <$> getArgs 
