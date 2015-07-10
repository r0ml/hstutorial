
import Data.List 

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

boxsize :: Int
boxsize =  3

values :: [Value]
values =  ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- Solvable only using the basic rules:
easy :: Grid
easy = ["2....1.38", "........5", ".7...6...",
        ".......13", ".981..257", "31....8..",
        "9..8...2.", ".5..69784", "4..25...."]

-- First gentle example from sudoku.org.uk:
gentle :: Grid
gentle = [".1.42...5", "..2.71.39", ".......4.",
          "2.71....6", "....4....", "6....74.3",
          ".7.......", "12.73.5..", "3...82.7."]

-- First diabolical example:
diabolical :: Grid
diabolical =  [".9.7..86.", ".31..5.2.", "8.6......",
               "..7.5...6", "...3.7...", "5...1.7..",
               "......1.9", ".2.6..35.", ".54..8.7."]

-- First "unsolvable" (requires backtracking) example:
unsolvable :: Grid
unsolvable = ["1..9.7..3", ".8.....7.", "..9...6..",
              "..72.94..", "41.....95", "..85.43..",
              "..3...7..", ".5.....4.", "2..8.6..9"]

-- Minimal sized grid (17 values) with a unique solution:
minimal :: Grid
minimal = [".98......", "....7....", "....15...",
           "1........", "...2....9", "...9.6.82",
           ".......3.", "5.1......", "...4...2."]

s99 :: Grid
s99 = ["..169.5..", "4..27...1", ".7.....9.",
       ".......3.", "...43...7", "...78.6..",
       "..6...8.5", ".2.14..6.", ".1.35..4."]

hnef :: Grid
hnef = ["....6..8.", ".2.......", "..1......",
        ".7....1.2", "5...3....", "......4..",
        "..42.1...", "3..7..6..", ".......5."]

htst :: Grid
htst = [".5..6...1", "..48...7.", "8......52",
        "2...57.3.", ".........", ".3.69...5",
        "79......8", ".1...65..", "5...3..6."]


-- Empty grid:
blank :: Grid
blank = replicate n (replicate n '.') where n = boxsize ^ 2 :: Int

-- Extracting rows, columns and boxes
-- ----------------------------------

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
  where pack = split . map split
        split = chop boxsize
        unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Validity checking
-- -----------------

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

-- A basic solver
-- --------------

{-
The function choices replaces blank squares in a grid by all possible
values for that square, giving a matrix of choices:
-}

type Choices = [Value]
choices :: Grid -> Matrix Choices
choices = map (map choice) where choice v = if empty v then values else [v]

{-
Reducing a matrix of choices to a choice of matrices can be defined 
in terms of the normal cartesian product of a list of lists, which
generalises the cartesian product of two lists:
-}

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

{-
For example, cp [[1,2],[3,4],[5,6]] gives:

   [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
-}

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

-- Finally, we can now specify a suduku solver:

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

-- Pruning the search space
-- ------------------------

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss] where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

-- Repeatedly pruning
-- ------------------

solve3 :: Grid -> [Grid]
solve3 = filter valid . collapse . fix prune . choices

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x' where x' = f x

-- Properties of matrices
-- ----------------------

complete :: Matrix Choices -> Bool
complete = all (all single)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all consistent (rows cm) && all consistent (cols cm) && all consistent (boxs cm)

consistent :: Row Choices -> Bool
consistent = nodups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)


-- Making choices one at a time
-- ----------------------------
{-
Clearly, a blocked matrix cannot lead to a solution.  However, our
previous solver does not take account of this.  More importantly,
a choice that leads to a blocked matrix can be duplicated many
times by the collapse function, because this function simply
considers all possible combinations of choices.  This is the 
primary source of inefficiency in our previous solver.

This problem can be addressed by expanding choices one square at
a time, and filtering out any resulting matrices that are blocked
before considering any further choices.  Implementing this idea
is straightforward, and gives our final Sudoku solver:
-}

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
 | blocked m  = []
 | complete m = collapse m
 | otherwise  = [g | m' <- expand m , g  <- search (prune m')]

-- The function expand behaves in the same way as collapse, except that
-- it only collapses the first square with more than one choice:

expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where (rows1,row:rows2) = break (any (not . single)) m
         (row1,cs:row2)    = break (not . single) row
{-
Note that there is no longer any need to check for valid grids at 
the end, because the process by which solutions are constructed 
guarantees that this will always be the case.  There also doesn't
seem to be any benefit in using "fix prune" rather than "prune"
above; the program is faster without using fix.  In fact, our
program now solves any newspaper Sudoku puzzle in an instant!

Exercise: modify the expand function to collapse a square with the
smallest number of choices greater than one, and see what effect 
this change has on the performance of the solver.
-}

-- Testing
-- -------

main :: IO ()
main = do 
  mapM_ (\x -> putStrLn (unlines (head (solve4 x))) >> putStrLn "") [ s99, htst, hnef]

  

