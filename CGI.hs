
import Preface.R0ml

pqs :: [(String,String)]->[(String,String)]
pqs x = 
     let Just a = lookup "QUERY_STRING" x
         f = break (=='&') . tail . snd
         b = map fst $ tail $ takeUntil ( null . snd) (iterate f ("",'&':a))
         c = map (second (drop 1) . break (=='=')) b
      in c
    where second f (x,y) = (x, f y)
          takeUntil _ [] = []
          takeUntil p (x:xs) = if not (p x) then x : takeUntil p xs else [x]

parm x y = maybe "UNKNOWN" id $ lookup x (pqs y)

simple f x y = writeResponse y (f x) >> return [("content-type", "text/plain")]


