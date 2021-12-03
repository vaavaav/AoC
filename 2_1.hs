import Data.List
import System.Environment 

f :: (Int,Int) -> (String, Int) -> (Int,Int)
f (h,v) ("forward", n) = (h+n,v  )
f (h,v) ("up", n )     = (h  ,v-n)
f (h,v) ("down", n)    = (h  ,v+n)


main = print 
     . uncurry (*)
     . foldl f (0,0)
     . map ((\[s,n] -> (s, read n)) . words)
     . lines
     =<< readFile 
     =<< head <$> getArgs 
