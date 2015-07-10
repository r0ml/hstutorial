
import Data.List (unfoldr)

{-
at = {w + (rho w) take (-aa) take a }
avl = { (iota disclose rho w) tilde w times disclose a [] aa }
box = { w slashbar w slash w w rho iota w star 2 }
cmap = { enclose [ iota rho rho w ] 1 epsilon each w jot . = w }
emt = { (, w = 0) / , iota rho w }
pvec = { (a ( aa avl) w ) ( a at) each enclose w}
pvex = { disclose , / a jot ( aa pvec) each w }
rcb = { ( iota w ) , each box disclose w star divide 2 }
svec = { disclose (cmap rcb rho w) pvex / (emt w), enclose enclose w} 
-}

chunk :: Int -> [e] -> [[e]]
chunk i ls = map (take i) (chunker ls [])  where
        chunker [] n = n
        chunker l n = l : chunker (drop i l) n

chunks :: Int -> [e] -> [[e]]
chunks = (takeWhile (not . null) .) . unfoldr . (Just .) . splitAt

{-
type Rcb = ((Int, Int), Int)
cmpx :: Rcb -> Rcb -> Bool
cmap :: [[Rcb]] -> [[ [[Bool]] ]] 
-}

svec p = foldr pvex [p] (emt p) where 
-- svec = liftM2 (foldr pvex) return emt
  pzs = length p
  pzq = (floor . sqrt . fromIntegral ) pzs
  rn = [1..pzs]
  emt = map fst . filter ( (0 ==) . snd) . zip [ (x,y) | x <- rn, y <- rn] . concat
  pvex c = concatMap (pvec c)
  pvec c p = map ( at c p) (avl c p)
  at (x,y) n v = let (a,b) = splitAt (x-1) n
                     (c,d) = splitAt (y-1) (head b)
                  in a ++ ( ( c++ ( v : tail d)) : tail b)
  cmx = cmap rcb 
  avl (x,y) = avlx (cmx !! (x-1) !! (y-1) )
  avlx r p = let ne = map snd . filter (uncurry ((. (0 /=)) . (&&))) $ zip (concat r) (concat p) in filter (`notElem` ne) rn
  cmap r =  [ [(map . map . cmpx) y r | y <- x] | x <- r ]
  cmpx ((x,y),z) ((p,q),r) = x == p || y == q || z == r
  rcb = zipWith zip (chunks pzs [ (x,y) | x <- rn, y <- rn ]) box
  box = let f = concatMap (replicate pzq) in f . map f . chunks pzq $ rn

type Grid = [[Int]]

s44 :: Grid
s44 = [[0,0,0,0],[0,0,2,1],[3,0,0,4],[0,0,0,0]]

s99 :: Grid
s99 = [[0,0,1,6,9,0,5,0,0],[4,0,0,2,7,0,0,0,1],[0,7,0,0,0,0,0,9,0],
       [0,0,0,0,0,0,0,3,0],[0,0,0,4,3,0,0,0,7],[0,0,0,7,8,0,6,0,0],
       [0,0,6,0,0,0,8,0,5],[0,2,0,1,4,0,0,6,0],[0,1,0,3,5,0,0,4,0]]

hnef :: Grid
hnef = [[0,0,0,0,6,0,0,8,0],[0,2,0,0,0,0,0,0,0],[0,0,1,0,0,0,0,0,0],
        [0,7,0,0,0,0,1,0,2],[5,0,0,0,3,0,0,0,0],[0,0,0,0,0,0,4,0,0],
        [0,0,4,2,0,1,0,0,0],[3,0,0,7,0,0,6,0,0],[0,0,0,0,0,0,0,5,0]]

htst :: Grid
htst = [[0,5,0,0,6,0,0,0,1],[0,0,4,8,0,0,0,7,0],[8,0,0,0,0,0,0,5,2],
        [2,0,0,0,5,7,0,3,0],[0,0,0,0,0,0,0,0,0],[0,3,0,6,9,0,0,0,5],
        [7,9,0,0,0,0,0,0,8],[0,1,0,0,0,6,5,0,0],[5,0,0,0,3,0,0,6,0]]

main :: IO ()
main = do
        print "----"       
        print $ svec s99
        print "----"       
        print $ svec htst
        print "----"       
        print $ svec hnef
        print "----"       

