import Data.List
import System.Environment 

solve = product
      . map sum
      . transpose
      . snd
      . mapAccumL f 0
      where f aim ('f',n) = (aim  , [n, aim*n])
            f aim ('u',n) = (aim-n, [0,0])
            f aim ('d',n) = (aim+n, [0,0])

parse = map ((\[s,n] -> (head s, read n)) . words)
      . lines

main = print . solve . parse =<< readFile . head =<< getArgs 
