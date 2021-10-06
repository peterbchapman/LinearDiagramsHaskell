{- File for linear diagram rules.
AUTHOR = PETER CHAPMAN
Started in February 2017.
-}

module LinearRules
(
sortD,
sortDForce,
sortDGlobalForce,
sortDGlobalForce2,
sortDAll,
sSortAll,
qSortAll,
sSortMin,
sSortMax,
qSortMin,
qSortMax,
segs,
segScore,
segMax,
setTotals,
verReorder,
horReorder,
horReorderGen,
edgesStr
)
where
 
import LinearFoundations



{- Functions to actually do stuff with diagrams -}


{- RULES THAT WORK FOR ALL TYPES OF DIAGRAM. SOUND -}

-- A normal form for diagrams. We reorder the labels to alphabetical order (and all of the lines within the overlaps). Then, we sort the overlaps.
normalForm :: (Ld a) => a -> a
normalForm d = mkD (quickSort (names d)) (quickSort (map (\ (ids,n) -> (reorder (getAlphaOrder (names d)) ids,n)) (overlaps d)))

-- reorder overlaps. Horizontal total ordering.
horReorderGen :: (Ld a) => [Int] -> a -> a
horReorderGen is d = mkD (names d) (reorder is (overlaps d))

-- reorder overlaps. Works on a pair. i j will swap overlaps i and j.
horReorder :: (Ld a) => Int -> Int -> a -> a
horReorder i j d = horReorderGen (switchInd i j [1..length (overlaps d)]) d


-- reorder labels. Vertical total ordering, by index.
verReorderGen :: (Ld a) => [Int] -> a -> a
verReorderGen is d = mkD (reorder is (names d)) (map (\ (ids,n) -> (reorder is ids,n)) (overlaps d))

-- reorder labels. Vertical swap, by index.
verReorder :: (Ld a) => Int -> Int -> a -> a
verReorder i j d = verReorderGen (switchInd i j [1..length (names d)]) d

-- reorder labels. Vertical swap, by name.
verReorderName :: (Ld a) =>  Label -> Label -> a -> a
verReorderName a b d = verReorder (find a (names d)) (find b (names d)) d


-- function to deal with merging overlaps where the IDs are the same
mergeO :: [Overlap] -> [Overlap]
mergeO [] = []
mergeO (o:os) = (fst o, foldl (\ acc (l,n) -> if l == fst o then acc + n else acc) (snd o) os) : mergeO os

-- function that will remove any duplicated overlaps, where the IDs are the same. Keeps the FIRST instance
remDups :: [Overlap] -> [Overlap]
remDups xs = reverse (remDups' (reverse xs))

-- helper function for above.
remDups' :: [Overlap] -> [Overlap]
remDups' [] = []
remDups' (o:os)
        | elem (fst o) (map fst os) = remDups' os
        | otherwise = o : remDups' os


-- remove line. Given a label, we remove it from the diagram, merge any overlaps, then remove duplicates. Deals with the sizes, even if of type LD or LOD.
removeL :: (Ld a) => Label -> a -> a
removeL l d = mkD (removeAt i (names d)) (remDups (mergeO (zip (map (removeAt i) (map fst (overlaps d))) (map snd (overlaps d)))))
        where i = find l (names d)

-- remove line, by index
removeLI :: (Ld a) => Int -> a -> a
removeLI i d = mkD (removeAt i (names d)) (remDups (mergeO (zip (map (removeAt i) (map fst (overlaps d))) (map snd (overlaps d)))))

-- remove an overlap, by index
removeO :: (Ld a) => Int -> a -> a
removeO i d = mkD (names d) (removeAt i (overlaps d))


{- RULES THAT WORK FOR ALL TYPES OF DIAGRAMS. UNSOUND -}

-- add a set of overlaps to a diagram
addO :: (Ld a) => [Overlap] -> a -> a
addO o d = mkD (names d) (overlaps d ++ o)

-- add a line to a diagram. We also need a list of whether or not to add a segment to each overlap
addL :: (Ld a) => Label -> [IDs] -> a -> a
addL l ids d = mkD (names d ++ [l]) (zipWith (\ id (o,n) -> (o ++ [id],n)) ids (overlaps d))


-- changing an overlap. Could copy the overlap, add it at end with just the offending bit changed, delete the original,
-- and move the new one into the old place. Instead, we'll do it directly.

-- flip a Pr to an Ab and vice versa in an overlap
-- flips the nth element in list
lineSegFlip :: Int -> [IDs] -> [IDs]
lineSegFlip 1 (x:xs) = notS x : xs 
lineSegFlip n (x:xs) = x : lineSegFlip (n-1) xs 

-- for an overlap
lineSFlip :: Int -> Overlap -> Overlap
lineSFlip n (os,m) = (lineSegFlip n os,m)

-- for the mth overlap in a list, flips the nth element of that overlap
lineSFlips :: Int -> Int -> [Overlap] -> [Overlap]
lineSFlips 1 n (o:os) = lineSFlip n o : os
lineSFlips m n (o:os) = o : lineSFlips (m-1) n os

-- for the mth overlap in a diagram, flips the nth element of that overlap
lineFlip :: (Ld a) => Int -> Int -> a -> a
lineFlip m n d = mkD (names d) (lineSFlips m n (overlaps d))

-- for the mth overlap in a diagram, flips the element corresponding to label l
lineFlipL :: (Ld a) => Int -> Label -> a -> a
lineFlipL m l d = lineFlip m (find l (names d)) d


{- SWITCHING DIAGRAM TYPES -}
lp2ld :: LP -> LD
lp2ld d = mkD (names d) (overlaps d)

ld2lod :: LD -> LOD
ld2lod d = mkD (names d) (overlaps d)

lp2lod :: LP -> LOD
lp2lod = ld2lod.lp2ld

lod2ld :: LOD -> LD
lod2ld d = mkD (names d) (overlaps d)

-- when translating to LP, we just ignore whatever was happening and set overlap sizes to 1.
ld2lp :: LD -> LP
ld2lp d = mkD (names d) (map (\ (o,_) -> (o,1)) (overlaps d))

lod2lp :: LOD -> LP
lod2lp = ld2lp.lod2ld



{- FUNCTIONS FOR "SORTING" A DIAGRAM ACCORDING TO FEWEST SEGMENTS. RECURSIVE GREEDY ALGORITHM -}

-- ### Works by finding the most "present" set in a diagram, then splitting into two sub-diagrams
-- based on that set. Then, repeats the process. Recombines sub-diagrams by checking which arrangement
-- (AB, BA, A(back B), (back B)A creates the most line matches.

-- the int will be the index of a label
howPr :: Int -> [Overlap] -> Int
howPr i [] = 0
howPr i (o:os) 
        | elemAt i (fst o) == Pr = 1 + (howPr i os) 
        | otherwise = howPr i os

-- above, but on a diagram with a label
howPrD :: (Ld a) => Label -> a -> Int
howPrD l d = howPr (find l (names d)) (overlaps d)

-- above, across the whole diagram
setTotals :: (Ld a) => a -> [Int]
setTotals d = map (\x -> howPrD x d) (names d)

-- returns the most "present" label in a diagram, from a given set of labels
mostPresent :: (Ld a) => Labels -> a -> Label
mostPresent ls d = snd $ last (quickSort (zip (map (\ l -> howPrD l d) ls) ls))

-- create a total ranking of the labels in a diagram
-- this will be passed to the sortDForce
rankLines :: (Ld a) => Labels -> a -> Labels
rankLines [] d = []
rankLines ns d = n: (rankLines (remove n ns) d)
    where n = mostPresent ns d



-- subdiagram. Give the diagram where the given label is present
subDPr :: (Ld a) => Label -> a -> a
subDPr l d = mkD (names d) (filter (\ (o,_) -> elemAt i o == Pr) (overlaps d))
        where i = find l (names d)

-- subdiagram. Give the diagram where the given label is present
subDAb :: (Ld a) => Label -> a -> a
subDAb l d = mkD (names d) (filter (\ (o,_) -> elemAt i o == Ab) (overlaps d))
        where i = find l (names d)

-- very unsafe and dumb adding of two diagrams. Will only be used when we know that the labels match.
addD :: (Ld a) => a -> a -> a
addD d1 d2 = mkD (names d1) (overlaps d1 ++ overlaps d2)

-- helper function for below. Only works when the two lists have the same length
matches' :: (Eq a) => [a] -> [a] -> Int
matches' [] [] = 0
matches' (x:xs) (y:ys)
        | x == y = 1 + matches' xs ys
        | otherwise = matches' xs ys

-- matches function. Checks two overlaps, and returns the number of entries where they both have Pr.
matches :: Overlap -> Overlap -> Int
matches (o1,_) (o2,_) = matches' o1 o2

-- match function. Takes two diagrams, and tells us whether the last overlap of the first, and the first overlap of the second, both have Pr in row l.
match :: (Ld a) => Label -> a -> a -> Bool
match l d1 d2 = elemAt (find l (names d1)) (fst (last (overlaps d1))) == Pr &&
                elemAt (find l (names d1)) (fst (last (overlaps d1))) == elemAt (find l (names d2)) (fst (head (overlaps d2)))

-- a more sophisticated adding function. Checks whether or not should be d1+d2 or d2+d1, with reversals as well.
addDPlus :: (Ld a) => a -> a -> a
addDPlus d1 d2
        | (overlaps d1) == [] || (overlaps d2) == [] = addD d1 d2
        | m1 >= maximum [m2,m3,m4]  = addD d1 d2
        | m2 >= maximum [m3,m4] = addD d2 d1
        | m3 >= m4 = addD (mkD (names d1) (reverse (overlaps d1))) d2
        | otherwise = addD d1 (mkD (names d2) (reverse (overlaps d2)))
        where m1 = matches (last $ overlaps d1) (head $ overlaps d2)
              m2 = matches (head $ overlaps d1) (last $ overlaps d2)
              m3 = matches (head $ overlaps d1) (head $ overlaps d2)
              m4 = matches (last $ overlaps d1) (last $ overlaps d2)


-- addDPlus, augmented with a label. This will force the sortDForce function to give precendence to a given label. If either diagram is empty, then it just
-- returns the two diagrams added.
addDPlusL :: (Ld a) => Label -> a -> a -> a
addDPlusL l d1 d2
        | (overlaps d1) == [] || (overlaps d2) == [] = addD d1 d2
        | m1 >= maximum [m2,m3,m4]  = addD d1 d2
        | m2 >= maximum [m3,m4] = addD d2 d1
        | m3 >= m4 = addD (mkD (names d1) (reverse (overlaps d1))) d2
        | otherwise = addD d1 (mkD (names d2) (reverse (overlaps d2)))
        where m1 = (match l d1 d2,matches (last $ overlaps d1) (head $ overlaps d2))
              m2 = (match l d2 d1,matches (head $ overlaps d1) (last $ overlaps d2))
              m3 = (match l (mkD (names d1) (reverse (overlaps d1))) d2,matches (head $ overlaps d1) (head $ overlaps d2))
              m4 = (match l d1 (mkD (names d2) (reverse (overlaps d2))),matches (last $ overlaps d1) (last $ overlaps d2))

-- sortD. Takes a list of labels and a diagram. Most of the time, the labels will be (names d).
sortD :: (Ld a) => Labels -> a -> a
sortD [] d = d
sortD ls d
        | length (overlaps d) <= 2 = d
        | otherwise = addDPlus (sortD (remove l ls) (subDPr l d)) (sortD (remove l ls) (subDAb l d))
                where l = mostPresent ls d

-- sortD with forcing. Takes an additional list of labels, and prioritises those ones first.
sortDForce :: (Ld a) => Labels -> Labels -> a -> a
sortDForce [] ns d = sortD ns d
sortDForce (l1:l2:ls) ns d
    | length (overlaps d) <= 2 = d
    | otherwise = addDPlusL l2 (sortDForce (l2:ls) (remove l1 ns) (subDPr l1 d))  (sortDForce (l2:ls) (remove l1 ns) (subDAb l1 d))
sortDForce (l:ls) ns d
    | length (overlaps d) <= 2 = d
    | otherwise = addDPlus (sortDForce ls (remove l ns) (subDPr l d))  (sortDForce ls (remove l ns) (subDAb l d))
    
-- sortD with forcing, second option. Takes an additional list of labels, but when recombining ignores the list.
sortDForce2 :: (Ld a) => Labels -> Labels -> a -> a
sortDForce2 [] ns d = sortD ns d
sortDForce2 (l:ls) ns d
    | length (overlaps d) <= 2 = d
    | otherwise = addDPlus (sortDForce2 ls (remove l ns) (subDPr l d))  (sortDForce2 ls (remove l ns) (subDAb l d))


-- sortD with global forcing. The additional list used is the total ranking
-- of the names in the diagram
sortDGlobalForce :: (Ld a) => a -> a
sortDGlobalForce d = sortDForce (rankLines (names d) d) (names d) d

-- sortD with global forcing, 2. When recombining, ignores the precedence list.
sortDGlobalForce2 :: (Ld a) => a -> a
sortDGlobalForce2 d = sortDForce2 (rankLines (names d) d) (names d) d


-- method for trying every single starting set to prioritise. Selects which one gives the lowest segment score
-- and returns that diagram. Where there are matching segment scores, will return whichever one is first.
sortDAll :: (Ld a) => a -> a
sortDAll d = sortDForce [l] (names d) d
    where l = elemAt (find (minimum scores) scores) (names d)
          scores = map segScore ds
          ds = map (\x -> (sortDForce [x] (names d) d)) (names d)



{- ANOTHER SORTING ALGORITHM -}
-- ## Like selection sort. Uses two lists of overlaps: the current state,
-- and the list of overlaps still to be added. Selects the best overlap still to be
-- added according to the "match" function above, and then adds it.

-- We don't need to worry about remove y from ys, as there should be no duplicate
-- overlaps in ys anyway. Looks at the end of xs, then determines which of the 
-- yet-to-be-used overlaps to add next
sSort' :: [Overlap] -> [Overlap] -> [Overlap]
sSort' xs [] = xs
sSort' xs ys = sSort' (xs ++ [y]) (remove y ys)
    where y = bestMatch (last xs) ys 

-- determines most matching overlap from a list, based on a given overlap
bestMatch :: Overlap -> [Overlap] -> Overlap
bestMatch x ys = elemAt n ys
    where n = find (maximum scores) scores
          scores = map (matches x) ys
    
-- version of the select sort algorithm that starts with whatever the first overlap is  
sSort :: (Ld a) => a -> a
sSort d = mkD (names d) (sSort' [o] os)
    where o = head (overlaps d)
          os = tail (overlaps d)
          
-- version of the select sort algorithm that takes an overlap as an input, and starts
-- with that
sSort2 :: (Ld a) => a -> Overlap -> a
sSort2 d o = mkD (names d) (sSort' [o] os)
    where os = remove o (overlaps d)
    
-- use the version above to search all possible starting overlaps, and return the one
-- with the lowest segment score
generateSelectionScores :: (Ld a) => a -> [Int]
generateSelectionScores d = map segScore ds
    where ds = map (sSort2 d) (overlaps d)

-- simple check to see if d has 0 or 1 overlaps. If it does, then just return
-- d. Otherwise there will be an issue with calling segCount' with a list
-- that has fewer than 2 elements, causing a problem. 
sSortAll :: (Ld a) => a -> a
sSortAll d = if (length (overlaps d) <= 1) then (d) else (sSort2 d o)
    where o = elemAt (find (minimum ds) ds) (overlaps d)
          ds = generateSelectionScores d
          

-- algorithm that looks for a "minimal" starting overlap. i.e. one with
-- the fewest number of lines present in it.
sSortMin :: (Ld a) => a -> a
sSortMin d = if (length (overlaps d) <= 1) then (d) else (sSort2 d o)
    where o = elemAt (find (minimum ps) ps) (overlaps d)
          ps = map (\x -> length (filter (==Pr) x)) (map fst (overlaps d))


-- algorithm that looks for a "maximal" starting overlap. i.e. one with 
-- the largest number of lines present in it.
sSortMax :: (Ld a) => a -> a
sSortMax d = if (length (overlaps d) <= 1) then (d) else (sSort2 d o)
    where o = elemAt (find (maximum ps) ps) (overlaps d)
          ps = map (\x -> length (filter (==Pr) x)) (map fst (overlaps d))

{- ANOTHER SORTING ALGORITHM.-}
-- ### Like sSort above, except that we can add new overlaps
-- at the start and the end of the list. So, treating it like a 
-- queue.

qSort' :: [Overlap] -> [Overlap] -> [Overlap]
qSort' xs [] = xs
qSort' xs ys
    | (maxR xs ys) >= (maxL xs ys) = qSort' (xs ++ [y]) (remove y ys)
    | otherwise = qSort' (z:xs) (remove z ys)
  where maxR xs ys = maximum (map (matches (last xs)) ys)
        maxL xs ys = maximum (map (matches (head xs)) ys)
        y = bestMatch (last xs) ys
        z = bestMatch (head xs) ys 

-- version of the queue sort algorithm that starts with whatever the first overlap is  
qSort :: (Ld a) => a -> a
qSort d = mkD (names d) (qSort' [o] os)
    where o = head (overlaps d)
          os = tail (overlaps d)
          
-- version of the queue sort algorithm that takes an overlap as an input, and starts
-- with that
qSort2 :: (Ld a) => a -> Overlap -> a
qSort2 d o = mkD (names d) (qSort' [o] os)
    where os = remove o (overlaps d)

-- use the version above to search all possible starting overlaps, and return the one
-- with the lowest segment score
generateQueueScores :: (Ld a) => a -> [Int]
generateQueueScores d = map segScore ds
    where ds = map (qSort2 d) (overlaps d)

-- if the diagram has one or 0 overlaps, then just return the diagram
-- Otherwise, it eventually causes segCount' to be called on a single
-- element list, or the empty list.    
qSortAll :: (Ld a) => a -> a
qSortAll d = if (length (overlaps d) <= 1) then (d) else (qSort2 d o)
    where o = elemAt (find (minimum ds) ds) (overlaps d)
          ds = generateQueueScores d

-- version that picks the minimum overlap to start, like sSortMin
qSortMin :: (Ld a) => a -> a
qSortMin d = if (length (overlaps d) <= 1) then (d) else (qSort2 d o)
    where o = elemAt (find (minimum ps) ps) (overlaps d)
          ps = map (\x -> length (filter (==Pr) x)) (map fst (overlaps d))
          
-- version that picks the maximum overlap to start, like sSortMax
qSortMax :: (Ld a) => a -> a
qSortMax d = if (length (overlaps d) <= 1) then (d) else (qSort2 d o)
    where o = elemAt (find (maximum ps) ps) (overlaps d)
          ps = map (\x -> length (filter (==Pr) x)) (map fst (overlaps d))



{- MORE SORTING ALGORITHMS -}
-- ### Like the forcing algorithms, except using
-- sSort and qSort. Works with 0, 1 or 2 sets. In
-- the case of 0 sets, it is just the normal sorting algoritm.

-- sSort with one set. Simply breaks the diagram in two, works on the part with
-- the label, and then continues adding remaining overlaps
sSortForce1 :: (Ld a) => Label -> a -> a
sSortForce1 l d = mkD (names d) (sSort' as nas)
    where as = overlaps (sSortAll (subDPr l d))
          nas = overlaps (subDAb l d)

-- same as above, but using qSort for each part
qSortForce1 :: (Ld a) => Label -> a -> a
qSortForce1 l d = mkD (names d) (qSort' as nas)
    where as = overlaps (qSortAll (subDPr l d))
          nas  = overlaps (subDAb l d)

-- version with two lines. Will work left to right, starting with
-- overlaps that are l1 - l2, then l1 \cap l2, then l2 - l1, then 
-- the remainder
sSortForce2 :: (Ld a) => Label -> Label -> a -> a
sSortForce2 l1 l2 d = mkD (names d) (sSort' (sSort' (sSort' set1 set2)  set3) set4)
    where set1 = overlaps (sSortAll (subDAb l2 (subDPr l1 d)))
          set2 = overlaps (subDPr l2 (subDPr l1 d))
          set3 = overlaps (subDAb l1 (subDPr l2 d))
          set4 = overlaps (subDAb l1 (subDAb l2 d))
          
-- qSort version of above. Will have to use sSort at times to 
-- ensure that some overlaps are not added in the wrong place
qSortForce2 :: (Ld a) => Label -> Label -> a -> a
qSortForce2 l1 l2 d = mkD (names d) (qSort' (sSort' (sSort' set1 set2)  set3) set4)
    where set1 = overlaps (qSortAll (subDAb l2 (subDPr l1 d)))
          set2 = overlaps (subDPr l2 (subDPr l1 d))
          set3 = overlaps (subDAb l1 (subDPr l2 d))
          set4 = overlaps (subDAb l1 (subDAb l2 d))


-- PREPARATION FOR TSP SOLVER.
-- create an n x n matrix of Hamming distances, using matches
-- rows then columns, but will be symmetrical
edgeMatrix :: (Ld a) => a -> [[Int]]
edgeMatrix d = map (\o -> map (unmatches o) (overlaps d)) (overlaps d)
    where unmatches o1 o2 = length (fst o1) - matches o1 o2


-- very inefficient, but idiot-proof
flatten :: [String] -> String
flatten [x] = x
flatten (x:xs) = x ++ flatten xs

strEdgeW :: [Int] -> String
strEdgeW [] = ""
strEdgeW (x:xs) = (show x ++ " ") ++ strEdgeW xs

strEdgeWeights :: [[Int]] -> [String]
strEdgeWeights = map strEdgeW 


-- string we need to pass in to the solver
edgesStr :: (Ld a) => a -> [String]
edgesStr = strEdgeWeights.edgeMatrix




-- Only makes sense for LP. Resizing an overlap
changeLength :: Int -> Int -> [Overlap] -> [Overlap]
changeLength 1 n (o:os) = (fst o, n) : os
changeLength m n (o:os) = o : changeLength (m-1) n os

-- Resizes the ith overlap to have length n. If the size is to become 0, it just deletes the overlap instead.
resize :: Int -> Int -> LP -> LP
resize i n (LP ns os) 
      | n <= 0 = LP ns (removeAt i os)
      | otherwise = LP ns (changeLength i n os)
      
      

      
      
{- FUNCTIONS FOR COUNTING THE NUMBER OF SEGMENTS IN A DIAGRAM -}
-- these are relatively fiddly, because you have to count along each set, rather than overlap, which
-- is not directly accessible. 

-- start by counting the number of Pr segments in a list of IDs
-- helper function that will undercount. Requires a starting list of at least size 2.
segCount' :: [IDs] -> Int
segCount' [Pr,Ab] = 0 -- if this is the entirety, there will be undercount
segCount' [Ab,Ab] = 0
segCount' [Pr,Pr] = 0 -- if this is the entirety, there will be undercount
segCount' [Ab,Pr] = 1
segCount' (Pr:Pr:os) = segCount' (Pr:os)
segCount' (Ab:Ab:os) = segCount' (Ab:os)
segCount' (Pr:Ab:os) = segCount' (Ab:os)
segCount' (Ab:Pr:os) = 1 + segCount' (Pr:os)

-- actual function. Checks whether the head of the input is Pr, and if so, adds 1
segCount :: [IDs] -> Int
segCount os = if head os == Pr then 1 + segCount' os else segCount' os


-- transpose a diagram so we look at it by row, rather than column
-- unsafe, but only gets used when we know the indices will work
getElems :: [Overlap] -> Int -> [IDs]
getElems [o] i = [elemAt i (fst o)]
getElems (o:os) i = elemAt i (fst o) : (getElems os i)

-- transpose function. Will ignore the set name, as this is not needed
transpose :: (Ld a) => a -> [[IDs]]
transpose d = map (getElems (overlaps d)) [1..(length (names d))]

-- create the list of integers representing number of segments used
-- for each set. Will be used to create both the segScore, and segMax
segs :: (Ld a) => a -> [Int]
segs d = map segCount (transpose d)

-- finally, count the number of segments in a diagram
segScore :: (Ld a) => a -> Int
segScore = sum.segs 

-- also work out the maximum number of segments in any one line
segMax :: (Ld a) => a -> Int
segMax = maximum.segs



{- TEST DIAGRAMS -}
d1 = LP ["a","b","c"] [([Pr,Ab,Pr],1),([Pr,Pr,Ab],10),([Pr,Pr,Ab],6),([Pr,Pr,Pr],7),([Pr,Ab,Ab],4)]

d2 = LD ["a","b","c","d"] [([Pr,Pr,Ab,Pr],1),([Ab,Pr,Pr,Pr],1),([Ab,Ab,Pr,Pr],1),([Pr,Ab,Ab,Pr],1),([Ab,Pr,Ab,Ab],1),([Ab,Pr,Ab,Pr],1),([Pr,Ab,Ab,Ab],1)]

venn4 = LD ["a","b","c","d"] [([Pr,Pr,Pr,Pr],1),([Pr,Pr,Pr,Ab],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Ab,Ab],1),([Pr,Ab,Pr,Pr],1),([Pr,Ab,Pr,Ab],1),([Pr,Ab,Ab,Pr],1),([Pr,Ab,Ab,Ab],1),([Ab,Pr,Pr,Pr],1),([Ab,Pr,Pr,Ab],1),([Ab,Pr,Ab,Pr],1),([Ab,Pr,Ab,Ab],1),([Ab,Ab,Pr,Pr],1),([Ab,Ab,Pr,Ab],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Ab,Ab],1)]

d3 = LP ["a","b","c"] [([Pr,Ab,Pr],1),([Pr,Pr,Ab],10),([Ab,Ab,Pr],6),([Ab,Pr,Pr],7),([Pr,Pr,Pr],4)]

d4 = LP ["a","b","c","d"] [([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],10),([Ab,Pr,Pr,Pr],7),([Pr,Ab,Pr,Ab],1),([Pr,Pr,Ab,Ab],10),([Ab,Pr,Pr,Ab],7)]

-- impossible to draw this using single segments for every set
d6 = LD ["a","b","c"] [([Pr,Pr,Ab],1),([Pr,Ab,Pr],1),([Ab,Pr,Pr],1)]

d7 = LD ["a","b","c","d","e","f"] [([Pr,Pr,Ab,Pr,Pr,Pr],1),([Pr,Ab,Pr,Pr,Pr,Pr],1),([Ab,Pr,Pr,Pr,Pr,Pr],1),([Pr,Pr,Pr,Pr,Pr,Ab],1),([Pr,Pr,Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab,Pr,Pr],1)]

d8 = LD ["a","b","c","d","e","f"] [([Pr,Pr,Ab,Pr,Ab,Pr],1),([Pr,Ab,Pr,Ab,Pr,Pr],1),([Ab,Pr,Pr,Pr,Pr,Ab],1),([Pr,Ab,Pr,Pr,Pr,Ab],1),([Ab,Pr,Pr,Pr,Ab,Pr],1),([Pr,Pr,Ab,Ab,Pr,Pr],1)]

d9 = LD ["a","b","c","d"] [([Pr,Pr,Ab,Ab],1),([Pr,Ab,Pr,Ab],1),([Ab,Pr,Pr,Ab],1),([Pr,Ab,Ab,Pr],1),([Ab,Pr,Ab,Pr],1),([Ab,Ab,Pr,Pr],1)]

d10 = LD ["a","b","c","d"] [([Pr,Pr,Ab,Ab],1),([Pr,Ab,Pr,Ab],1),([Ab,Pr,Pr,Ab],1),([Pr,Ab,Ab,Pr],1),([Ab,Pr,Ab,Pr],1),([Ab,Ab,Pr,Pr],1),([Ab,Pr,Pr,Pr],1),([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab],1)]

d11 = LD ["a","b","c","d"] [([Pr,Pr,Ab,Ab],1),([Pr,Ab,Pr,Ab],1),([Ab,Pr,Pr,Ab],1),([Pr,Ab,Ab,Pr],1),([Ab,Pr,Ab,Pr],1),([Ab,Ab,Pr,Pr],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Pr,Ab],1),([Ab,Pr,Ab,Ab],1),([Pr,Ab,Ab,Ab],1)]

d12 = LD ["a","b","c","d"] [([Ab,Pr,Pr,Pr],1),([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Pr,Ab],1),([Ab,Pr,Ab,Ab],1),([Pr,Ab,Ab,Ab],1)]

d13 = LD ["a","b","c","d"] [([Ab,Pr,Pr,Pr],1),([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Pr,Ab],1),([Ab,Pr,Ab,Ab],1)]

d14 = LD ["a","b","c","d"] [([Ab,Pr,Pr,Pr],1),([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Pr,Ab],1)]

d15 = LD ["a","b","c","d"] [([Pr,Ab,Pr,Pr],1),([Pr,Pr,Ab,Pr],1),([Pr,Pr,Pr,Ab],1),([Ab,Ab,Ab,Pr],1),([Ab,Ab,Pr,Ab],1),([Ab,Pr,Ab,Ab],1)]

d16 = LD ["a","b","c","d"] [([Pr,Ab,Ab,Ab],1),([Pr,Pr,Ab,Ab],1),([Pr,Pr,Pr,Ab],1),([Pr,Pr,Pr,Pr],1),([Ab,Pr,Pr,Pr],1),([Ab,Ab,Pr,Pr],1),([Ab,Ab,Ab,Pr],1),([Ab,Pr,Pr,Ab],1)]


os1 = [Pr,Pr,Ab,Pr,Ab,Pr] -- should give segCount of 3
os2 = [Ab,Pr,Pr,Pr,Ab] -- should give segCount of 1
os3 = [Ab,Ab,Ab,Ab] -- should give segCount of 0

        



