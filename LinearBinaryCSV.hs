{- File for input from binary (0,1) CSV file to create Linear Diagram
The purpose is to determine best drawing algorithms
 AUTHOR = Peter Chapman
 Started October 2020
-}

import LinearFoundations
import LinearRules
import Venn
import Text.ParserCombinators.Parsec
import CSV
import Text.Read
import Control.Monad
import System.Directory
import System.FilePath
import TSPLK


-- getting at the values. Not safe.
fromRight :: Either a [[String]] -> [[String]]
fromRight (Right xs) = xs

fromRightSingle :: Either a [String] -> [String]
fromRightSingle (Right xs) = xs


-- ####   FUNCTIONS FOR TURNING THE BINARY CSV INTO A DIAGRAM   ##### --
{-
The csv appears as a string of strings, whose entries are "1" or "0". For example:
[["1","1","0"],["0","1","1"],["0","1","0"]]
-}

-- First, create a set of dummy labels. These will be needed for the
-- diagram generation. Need to create as many labels as there are things in each
-- overlap. Calls them s1, s2, s3, s4,...
createNames :: [[String]] -> Labels
createNames xss = map (\ x -> "s" ++ show x) [1..(length (head xss))]


-- Relatively straightforward to create an overlap from a line of the csv.
createOverlap' :: [String] -> [IDs]
createOverlap' [] = []
createOverlap' (s:ss)
    | read s == 0 = Ab : createOverlap' ss
    | read s == 1 = Pr : createOverlap' ss

-- have dummy length of 1 for all overlaps    
createOverlaps :: [[String]] -> [Overlap]
createOverlaps ss = map (\ x -> (createOverlap' x,1)) ss


-- create the diagram. Just a non-LP diagram
createLD :: [[String]] -> LD
createLD cs = LD (createNames cs) (createOverlaps cs)



-- ### FUNCTIONS FOR ANALYSING THE DIAGRAM ### --
{-
Once we have a list of integers, then we can calculate mean, sd, and two
different measures of how different the numbers of lines are. How Venn-like
the diagram is.
-}
getMean :: [Int] -> Float
getMean xs = (sum ys) / n
    where ys = map fromIntegral xs
          n = fromIntegral (length xs)
          
getStdDev :: [Int] -> Float
getStdDev xs = sqrt ((sum $ map (\x -> (x - mu)^2) ys) / n)
    where ys = map fromIntegral xs
          n = fromIntegral (length xs)
          mu = getMean xs
          
getVennScoreAdj :: [Int] -> Float
getVennScoreAdj xs = (getStdDev xs) / (getMean xs)

getVennScore :: [Int] -> Float
getVennScore xs = (getStdDev xs) / n
    where n = fromIntegral (length xs)


-- ### I/O stuff ###
-- read in csv, create diagram, calculate property statistics, optimise then score
-- write to results.csv, each on a new line.
-- This works if *every* file in dir is a csv of the correct format
-- i.e. 1s and 0s. Otherwise it will break.

{-
-- version that just looks at the sortDAll, sortDGlobalForce, sortDGlobalForce2 and sortD
optimise :: FilePath -> FilePath -> IO()
optimise dir file = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res

    let myD = createLD strForm
    let score1 = segScore (sortD (names myD) myD)
    let score2 = segScore (sortDAll myD)
    let score3 = segScore (sortDGlobalForce myD)
    let score4 = segScore (sortDGlobalForce2 myD)
    let tots = setTotals myD
    let mean = getMean tots
    let sd = getStdDev tots
    let v1 = getVennScore tots
    let v2 = getVennScoreAdj tots 
    putStrLn (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4))
    appendFile "results.csv" (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4) ++ "," ++ (show mean) ++ "," ++ (show sd) ++ "," ++ (show v1) ++ "," ++ (show v2) ++ "\n")
-}

{-
-- version that just looks at the sSort and qSort versions
optimise :: FilePath -> FilePath -> IO()
optimise dir file = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res

    let myD = createLD strForm
    let score1 = segScore (sSortMin myD)
    let score2 = segScore (sSortMax myD)
    let score3 = segScore (sSortAll myD)
    let score4 = segScore (qSortMin myD)
    let score5 = segScore (qSortMax myD)
    let score6 = segScore (qSortAll myD)
    let tots = setTotals myD
    let mean = getMean tots
    let sd = getStdDev tots
    let v1 = getVennScore tots
    let v2 = getVennScoreAdj tots 
    putStrLn (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4)++ "," ++ (show score5)++ "," ++ (show score6))
    appendFile "results.csv" (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4) ++ "," ++ (show score5) ++ "," ++ (show score6)++ "," ++ (show mean) ++ "," ++ (show sd) ++ "," ++ (show v1) ++ "," ++ (show v2) ++ "\n")
-}


-- version that does everything
optimise :: FilePath -> FilePath -> IO()
optimise dir file = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res

    let myD = createLD strForm
    let score1 = segScore (sortD (names myD) myD)
    let score2 = segScore (sortDAll myD)
    let score3 = segScore (sortDGlobalForce myD)
    let score4 = segScore (sortDGlobalForce2 myD)
    let score5 = segScore (sSortMin myD)
    let score6 = segScore (sSortMax myD)
    let score7 = segScore (sSortAll myD)
    let score8 = segScore (qSortMin myD)
    let score9 = segScore (qSortMax myD)
    let score10 = segScore (qSortAll myD)
    let tots = setTotals myD
    let mean = getMean tots
    let sd = getStdDev tots
    let v1 = getVennScore tots
    let v2 = getVennScoreAdj tots 
    putStrLn (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4)++ "," ++ (show score5)++ "," ++ (show score6)++ "," ++ (show score7)++ "," ++ (show score8)++ "," ++ (show score9)++ "," ++ (show score10))
    appendFile "results.csv" (file ++ "," ++ (show score1) ++ "," ++ (show score2) ++ "," ++ (show score3) ++ "," ++ (show score4)++ "," ++ (show score5)++ "," ++ (show score6)++ "," ++ (show score7)++ "," ++ (show score8)++ "," ++ (show score9)++ "," ++ (show score10) ++ "," ++ (show mean) ++ "," ++ (show sd) ++ "," ++ (show v1) ++ "," ++ (show v2) ++ "\n")
    

    
-- function which runs through all of the csv files in dir
-- and applies the above function. The first two parts remove two "files"
-- the getDirectoryContents function returns.
optimiseAll :: FilePath -> IO()
optimiseAll dir = do
          files <- filter (/= "..")<$>(filter(/= ".")<$>(getDirectoryContents dir))
          forM_ files (optimise dir)





-- function which will read in the individual files from the EA/simulated annealing
-- output, then take the average and write to a file

-- function for getting the averages we need from Kevin's file
algScores :: [[String]] -> [Int]
algScores ss = map read (map last ss)

kevinScores :: FilePath -> FilePath -> IO()
kevinScores dir file = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res
    
    let mean = getMean (algScores strForm)
    putStrLn (file ++ "," ++ (show mean))
    appendFile "aveScores.csv" (file ++ "," ++ (show mean) ++ "\n")
    
kevinScoresAll :: FilePath -> IO()
kevinScoresAll dir = do
          files <- filter (/= "..")<$>(filter(/= ".")<$>(getDirectoryContents dir))
          forM_ files (kevinScores dir)

-- ### RUNNING THE LK ALGORITHM ###
 
-- dir is the directory of the input csv files
-- file is the name of the input csv file
-- outdir is where the paths will be stored: just uses the name of the input file
lk :: FilePath -> FilePath -> FilePath -> IO()
lk dir file outdir = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res

    let myD = createLD strForm
    let edgeS = edgesStr myD
    
    let filebase = takeBaseName file
    
    tour <- tsp defConfig edgeS (outdir</>filebase)
    let score1 = segScore (horReorderGen (newOrder tour) myD)
    putStrLn (file ++ "," ++ (show score1) ++ "\n") 
    







-- ### CHECKING VENN DIAGRAMS ###
-- version that does everything
checkVenn :: FilePath -> FilePath -> Int -> IO()
checkVenn dir file n = do
    res <- parseFromFile csvFile (dir</>file)
    let strForm = fromRight res
    
    let vn = venn n

    let myD = createLD strForm
    
    let equal = vn == myD
    let score = segMax myD
    let scores = segs myD
 
    putStrLn (file ++ "," ++ (show equal) ++ "," ++ (show score) ++ "," ++ (show scores))
    



-- ### Unneeded functions ###
-- create the matrix of 0s and 1s
createMatrix' :: [String] -> [Int]
createMatrix' [] =  []
createMatrix' (s:ss)
    | read s == 1 = 1:createMatrix' ss
    | read s == 0 = 0:createMatrix' ss
    
createMatrix :: [[String]] -> [[Int]]
createMatrix = map createMatrix'

lineTotals :: [[Int]] -> [Int]
lineTotals = map sum





