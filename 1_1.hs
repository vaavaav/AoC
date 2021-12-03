import Data.List
import System.Environment

main = print
     . length
     . filter (< 0)
     . (\l -> zipWith (-) l (tail l))
     . map read 
     . lines
     =<< readFile 
     =<< head <$> getArgs 
