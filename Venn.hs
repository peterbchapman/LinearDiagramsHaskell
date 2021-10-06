{- File checking Venn-n.
AUTHOR = PETER CHAPMAN
Started in April 2021.
-}
 
module Venn
(
venn
)
where 
 
import LinearRules
import LinearFoundations

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | rem n 2 == 1 = 1 : toBin (div n 2)
    | otherwise = 0 : toBin (div n 2)
    
pad0 :: Int -> [Int] -> [Int]
pad0 n r = take (n - length r) (repeat 0) ++ (reverse r)


-- assumes list of ints is ordered. Not a big restriction, since will be
-- used on [1..(2^n-1)]
binList :: [Int] -> [[Int]]
binList xs = map (pad0 n) (map toBin xs)
    where n = length $ toBin (last xs)


-- First, create a set of dummy labels. These will be needed for the
-- diagram generation. Need to create as many labels as there are things in each
-- overlap. Calls them s1, s2, s3, s4,...
createNames :: [[Int]] -> Labels
createNames xss = map (\ x -> "s" ++ show x) [1..(length (head xss))]


-- Relatively straightforward to create an overlap from a line of the csv.
createOverlap' :: [Int] -> [IDs]
createOverlap' [] = []
createOverlap' (s:ss)
    | s == 0 = Ab : createOverlap' ss
    | s == 1 = Pr : createOverlap' ss

-- have dummy length of 1 for all overlaps    
createOverlaps :: [[Int]] -> [Overlap]
createOverlaps ss = map (\ x -> (createOverlap' x,1)) ss


-- create the diagram. Just a non-LP diagram
createLD :: [[Int]] -> LD
createLD cs = LD (createNames cs) (createOverlaps cs)

venn :: Int -> LD
venn n = createLD $ binList [1..k]
    where k = 2^n - 1

