import Data.List
import System.Environment 

f :: ((Int,Int),Int) -> (String, Int) -> ((Int,Int),Int)
f ((h,v),a) ("forward", n) = ((h+n,v+a*n),a)
f ((h,v),a) ("up", n )     = ((h,v),a-n)
f ((h,v),a) ("down", n)    = ((h,v),a+n)


main = print 
     . (uncurry (*) . fst)
     . foldl f ((0,0),0)
     . map ((\[s,n] -> (s, read n :: Int)) . words)
     . lines
     =<< readFile 
     =<< head <$> getArgs 
