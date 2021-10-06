-- module to pass diagram to the TSP solver that
-- uses the Lin-Kernighan heuristic.

-- Heavily influenced by Algorithms.Concorde.LinKern
-- https://hackage.haskell.org/package/concorde
-- basically just rewrites the above so that a matrix of edges, rather
-- than Euclidean points, are used in the created .tsp file.
-- tidies up the output as well, to be specific to the purpose of this work.


{- 
This is where the solver comes from
http://www.math.uwaterloo.ca/tsp/concorde/downloads/downloads.htm
-}
module TSPLK
    ( -- * The heuristic
      tsp
      -- * Configuration
    , Config(..), defConfig
    ,newOrder -- list of ints to pass to horizontal reordering function
    ) where

import Control.Monad
import Control.Exception
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Temp
import Text.Printf
import Safe

--import qualified Data.IntMap    as IM
import qualified System.Process as P

errStr :: String -> String
errStr = ("TSPLK: " ++)

-- | Configuration for @'tsp'@.
data Config = Config
    {  -- | Path to the @linkern@ executable.  Searches @$PATH@ by default.
      executable :: FilePath
      -- | If set, write progress information to standard output.
    , verbose    :: Bool
      -- | Stop looking for better solutions after this many seconds.
    , timeBound  :: Maybe Double
      -- | Run this many optimization steps.  Default is the number of points.
    , steps      :: Maybe Int
      -- | Run this many separate optimizations.  Default is 1.
    , runs       :: Int
      -- | Other command-line arguments to the @linkern@ executable.
    , otherArgs  :: [String]
    } deriving (Eq, Ord, Read, Show)

-- | Default configuration.
defConfig :: Config
defConfig = Config
    { executable = "./../../concorde/LINKERN/linkern" -- obviously change this part
    , verbose    = False
    , timeBound  = Nothing
    , steps      = Nothing
    , runs       = 1
    , otherArgs  = [] }

-- | Approximate a solution to the Hamming TSP, used for determining
-- ordering of Linear Diagram overlaps, using the Lin-Kernighan heuristic.
--
-- Invokes Concorde's @linkern@ executable as an external process.
--
tsp
    :: Config     -- ^ Configuration.
    -> [String]   -- ^ Gives the matrix, as a list of strings, one for each row
    -> FilePath      -- ^ where to log the output
    -> IO [String]   -- ^ Produces points permuted in tour order, 0-based
tsp cfg edgeWeights path =
    -- Log to a temp file if not verbose.
    -- Hardcoded to log to path.txt. This will need to be changed.
    withSystemTempFile "log"    $ \_          logHdl    ->
    withSystemTempFile "coords" $ \coordsPath coordsHdl -> do

        -- Create .tsp file
        mapM_ (hPutStrLn coordsHdl)
            ([ "TYPE:TSP"
            , "DIMENSION:" ++ show (length edgeWeights)
            , "EDGE_WEIGHT_TYPE:EXPLICIT"
            , "EDGE_WEIGHT_FORMAT:FULL_MATRIX"
            , "NODE_COORD_TYPE:NO_COORDS"
            , "EDGE_WEIGHT_SECTION" ] ++ edgeWeights)
        hPutStrLn coordsHdl "EOF"
        hClose coordsHdl

        -- Invoke linkern
        let optArg flag fmt proj = case proj cfg of
                Nothing -> []
                Just x  -> [flag, printf fmt x]

            allArgs = concat [ ["-o", path]
                             , [coordsPath] ]

            subOut  = if verbose cfg then P.Inherit else P.UseHandle logHdl
            procCfg = (P.proc (executable cfg) allArgs) { P.std_out = subOut }

        (Nothing, Nothing, Nothing, procHdl) <- P.createProcess procCfg
        ec <- P.waitForProcess procHdl
        case ec of
            ExitSuccess   -> return ()
            ExitFailure n -> throwIO . ErrorCall . errStr $
                ("process exited with code " ++ show n ++ extra) where
                    extra | n == 127  = "; program not installed or not in path?"
                          | otherwise = ""

        -- Skip the first line, then read the first int of each remaining
        -- line as the index of the overlap. This is a 0-based index.
        lns <- lines `fmap` readFile path
        _   <- evaluate (length lns)
        

        return $ map (head. words) (drop 1 lns)
        

newOrder :: [String] -> [Int]
newOrder xs = map (+1) (map read xs)

