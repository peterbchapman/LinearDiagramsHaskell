{- 
Drawing linear diagrams. Uses a new type class LD a, which has three derived instances.
AUTHOR = P. Chapman
DATE = Started Feb 2017.
-}

module LinearFoundations
(
IDs(..),
Labels,
Label,
Overlap,
Ld(..),
LP(..),
LD(..),
LOD(..),
quickSort,
getAlphaOrder,
elemAt,
removeAt,
reorder,
removeDuplicates,
find,
switchInd,
notS,
remove,
getPointsD,
getMidPointsD
)
where

data IDs = Pr | Ab deriving (Read, Ord, Eq) -- whether a line is Present or Absent in an overlap

-- pretty printing for lines
instance Show IDs where 
        show Pr = "---"
        show Ab = "   "

type Label = String -- name of a set

type Labels = [Label] -- set names

type Overlap = ([IDs],Int) -- overlaps may contain size info. In some cases, this will be ignored.

-- new type class containing all the functions necessary to build and deconstruct diagrams
class Ld a where
        ld :: a -> Bool
        names :: a -> Labels
        overlaps :: a -> [Overlap]
        mkD :: Labels -> [Overlap] -> a

-- the constructors for the different linear diagram types.

{- LENGTH PROPORTIONAL -}
data LP = LP Labels [Overlap] 

instance Ld LP where 
        ld (LP ns os) = filter (\i -> (length.fst) i /= m) os == [] && -- all overlaps contain the correct number of slots
               filter (<= 0) (map snd os) == [] -- all overlaps have strictly positive size
                        where m = length ns
        names (LP ns _) = ns
        overlaps (LP _ os) = os
        mkD ns os = LP ns os

-- Two diagrams are equal to each other if they have the same normal form. LP version
instance Eq LP where
        LP ns1 os1 == LP ns2 os2 = 
                (quickSort ns1 == quickSort ns2) && 
                (quickSort (map (\ (ids,n) -> (reorder (getAlphaOrder ns1) ids,n)) os1)) == (quickSort (map (\ (ids,n) -> (reorder (getAlphaOrder ns2) ids,n)) os2))


-- overwriting Show with a pretty-printer. LP version
instance Show LP where
        show (LP ns os) = unlines $ (map (\i -> padBlank (1 + m - length (elemAt i ns)) (elemAt i ns) ++ "|" ++ showLine i os) [1..length ns]) ++ 
                ([padBlank (m+1) "" ++ "|" ++ showLengths os])
                where m = maximum (map length ns)






{- WITH EXISTENTIAL IMPORT -}
data LD = LD Labels [Overlap]

instance Ld LD where
        ld (LD ns os) = filter (\i -> (length.fst) i /= m) os == []  -- all overlaps contain the correct number of slots
                        where m = length ns
        names (LD ns _) = ns
        overlaps (LD _ os) = os
        mkD ns os = LD ns os

-- Two diagrams are equal to each other if they have the same normal form. LD version. Ignore the sizes of overlaps.
instance Eq LD where
        LD ns1 os1 == LD ns2 os2 = 
                (quickSort ns1 == quickSort ns2) && 
                (quickSort (map (reorder (getAlphaOrder ns1)) (map fst os1))) == (quickSort (map (reorder (getAlphaOrder ns2)) (map fst os2)))


-- overwriting Show with a pretty-printer. LD version. Ignores the sizes of overlaps.
instance Show LD where
        show (LD ns os) = unlines $ (map (\i -> padBlank (1 + m - length (elemAt i ns)) (elemAt i ns) ++ "|" ++ showLine i os) [1..length ns])
                where m = maximum (map length ns)







{- WITHOUT EXISTENTIAL IMPORT. FOR ONTOLOGY ENGINEERNG -}
data LOD = LOD Labels [Overlap]

instance Ld LOD where
        ld (LOD ns os) = filter (\i -> (length.fst) i /= m) os == []  -- all overlaps contain the correct number of slots
                        where m = length ns
        names (LOD ns _) = ns
        overlaps (LOD _ os) = os
        mkD ns os = LOD ns os

-- Two diagrams are equal to each other if they have the same normal form. LD version. Ignore the sizes of overlaps.
instance Eq LOD where
        LOD ns1 os1 == LOD ns2 os2 = 
                (quickSort ns1 == quickSort ns2) && 
                (quickSort (map (reorder (getAlphaOrder ns1)) (map fst os1))) == (quickSort (map (reorder (getAlphaOrder ns2)) (map fst os2)))


-- overwriting Show with a pretty-printer. LD version. Ignores the sizes of overlaps.
instance Show LOD where
        show (LOD ns os) = unlines $ (map (\i -> padBlank (1 + m - length (elemAt i ns)) (elemAt i ns) ++ "|" ++ showLine i os) [1..length ns])
                where m = maximum (map length ns)





{- functions used for pretty printing. These will help derive instances of Show, 
and will make the output readable from the command line. -}
-- beginnings of Show. Given an integer and a list of overlaps, returns the ith row.
showLine :: Int -> [Overlap] -> String
showLine n [] = ""
showLine n (o:os) = show (elemAt n (fst o)) ++ "|" ++ showLine n os

-- beginnings of Show. Given a list of overlaps, return the length of the overlaps in list form.
showLengths :: [Overlap] -> String
showLengths [] = ""
showLengths (o:os)
        | (length.show.snd) o == 1 = show (snd o) ++ "  |" ++ showLengths os
        | otherwise = show (snd o) ++ " |" ++ showLengths os

-- way to make sure the labels are all lined up
padBlank :: Int -> Label -> String
padBlank n lab = lab ++ take n (repeat ' ')






{- Additional functions needed to define the type class instances -}
-- we need quickSort
quickSort :: (Ord a, Eq a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (lessThan xs) ++ [x] ++ quickSort (greaterThan xs)
        where lessThan = filter (<= x) 
              greaterThan = filter (> x) 


-- Given some labels, return the permutation that will make them in alphabetical order
getAlphaOrder :: Labels -> [Int]
getAlphaOrder ls = map snd $ quickSort (zip ls [1..length ls])

-- helper function, return the element at index i in a list, starting at 1. Unsafe
elemAt :: Int -> [a] -> a
elemAt 1 (x:xs) = x
elemAt n (x:xs) = elemAt (n-1) xs

-- helper function, removes the element at index i in a list, starting at 1. Unsafe
removeAt :: Int -> [a] -> [a]
removeAt 1 (x:xs) = xs
removeAt n (x:xs) = x: removeAt (n-1) xs

-- helper function for partial reorderings (i.e. switches)
replaceAt :: Int -> a -> [a] -> [a]
replaceAt 1 y (x:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n-1) y xs

-- function to pass to the reorder function, to allow switches. Unsafe
switchInd :: Int -> Int -> [a] -> [a]
switchInd i j xs = replaceAt j (elemAt i xs) (replaceAt i (elemAt j xs) xs)

-- Apply reorder
reorder :: [Int] -> [a] -> [a]
reorder xs ys = foldl (\ acc x -> acc ++ [(elemAt x ys)]) [] xs

-- Remove duplicates v2. This keeps the LAST instance of any element
removeDuplicates2 :: (Eq a) => [a] -> [a]
removeDuplicates2 [] = []
removeDuplicates2 (x:xs)
        | elem x xs = removeDuplicates2 xs
        | otherwise = x: removeDuplicates2 xs

-- Remove duplicates v1. This keeps the FIRST instance of any element
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates xs = reverse (removeDuplicates2 (reverse xs))

-- find function. Unsafe
find :: (Eq a) => a -> [a] -> Int
find x (y:ys)
        | x == y = 1
        | otherwise = 1 + find x ys

notS :: IDs -> IDs
notS Pr = Ab
notS Ab = Pr


-- remove an element from a list. Only removes one instance
remove :: (Eq a) => a -> [a] -> [a]
remove x [] = []
remove x (y:ys)
        | x == y = ys
        | otherwise = y : remove x ys





-- functions that help with the drawing in Cairo.

-- working out the start points for a single overlap. Uses the length information from the overlap
getPoints :: Overlap -> (Double,Double) -> Double -> [(Double,Double)]
getPoints ([o],l) (x,y) h = [(x,y)]
getPoints ((o:os),l) (x,y) h = (x,y) : getPoints (os,l) (x,y+h) h

-- working out start points for a list of overlaps. Again, uses length info.
getPointsD' :: [Overlap] -> (Double,Double) -> Double -> [[(Double,Double)]]
getPointsD' [o] (x,y) h = [getPoints o (x,y) h]
getPointsD' (o:os) (x,y) h = getPoints o (x,y) h : getPointsD' os (x+fromIntegral (snd o),y) h

-- working out start points for a diagram, where top left line starts at (x,y), separated by h
getPointsD :: (Ld a) => a -> (Double,Double) -> Double -> [[(Double,Double)]]
getPointsD d (x,y) h = getPointsD' (overlaps d) (x,y) h

-- functions that help with drawing the buttons in Cairo.
-- working out the midpoints for a list of overlaps. Can then place the button in the middle of the overlap
getMidPointsD' :: [Overlap] -> (Double,Double) -> Double -> [[(Double,Double)]]
getMidPointsD' [o] (x,y) h = [getPoints o (x + fromIntegral (snd o)/2,y) h]
getMidPointsD' (o:os) (x,y) h = getPoints o (x + fromIntegral (snd o)/2,y) h : getMidPointsD' os (x+fromIntegral (snd o),y) h

-- working out start points for buttons for a diagram
getMidPointsD :: (Ld a) => a -> (Double,Double) -> Double -> [[(Double,Double)]]
getMidPointsD d (x,y) h = getMidPointsD' (overlaps d) (x,y) h



-- testing set
d1 = LP ["a","b","c"] [([Pr,Ab,Pr],1),([Pr,Pr,Ab],10),([Ab,Pr,Ab],6),([Ab,Pr,Pr],7),([Ab,Ab,Pr],4)]
o1 = [([Pr,Ab,Pr],1 :: Int),([Pr,Pr,Ab],10),([Ab,Pr,Ab],6),([Ab,Pr,Pr],7),([Ab,Ab,Pr],4)]



