
module BuildSimplexes
       (getOneSimplicies, getOneSimpliciesMatrices, getAllSimplicies, simplexHelper,
        ppSimp, ppToFiles)
       where

import           Combinations
import           Control.Arrow                     ((***))
import           Control.Monad
import           Data.Bifunctor                    (first)
import           Data.Function
import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP
import           Data.List
import qualified Data.Map                          as M
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector                       as V (fromList, modify,
                                                         toList)
import qualified Data.Vector.Algorithms.Intro      as I (sortBy)
import           LocalRegionEdgeSet
import           Numeric.LinearAlgebra             hiding ((<>))
import           TangentSpaces

getOneSimplicies
  :: Double
  -> Int
  -> Matrix Double
  -> SimpMode
  -> Either [String] (GP.Gr () ESKey)
getOneSimplicies n kdims input mode = simplex
  where nbdSize = floor $ fromIntegral kdims * n
        simplexSize = kdims + 1
        data' = toRows input
        edges' = buildEdgeSets nbdSize simplexSize data'
        simplex = simplComp mode edges'

getOneSimpliciesMatrices
  :: Double
  -> Int
  -> [Matrix Double]
  -> SimpMode
  -> Either [String] (GP.Gr () ESKey)
getOneSimpliciesMatrices n kdims input mode = simplex
  where nbdSize = floor $ fromIntegral kdims * n
        simplexSize = kdims + 1
        data' = map flatten input
        edges' = buildEdgeSets nbdSize simplexSize data'
        simplex = simplComp mode edges'

getAllSimplicies :: Int -> GP.Gr () ESKey -> [[Integer]]
getAllSimplicies k =
  ([] :) . concatMap (simplexHelper k) . vsort . setToIntegers . scc
  where setToIntegers = map . map $ toInteger
        vsort =
          V.toList .
          V.modify (I.sortBy (liftM2 (<>) (comparing length) compare)) .
          V.fromList
-- This sorting technique is used in case of emergencies. This is an
-- emergency situation I suppose.

-- getAllSimplicies :: Int -> GP.Gr () ESKey -> [[Node]]
-- getAllSimplicies k =
--   ([] :) .
--   takeWhile ((<= k) . length) .
--   concat . simplexHelper k . groupBy (on (==) length) . vsort . scc

simplexHelper :: (Ord a) => Int -> [a] -> [[a]]
simplexHelper maxComb x =  map sort . filter (not . null) $ concatMap (`choose` x) listOfCombs
  where listOfCombs = [1 .. maxComb]

simplexHelper' :: Int -> [[[a]]] -> [[[a]]]
simplexHelper' _ [] = []
simplexHelper' n (x:xs) =
  let lists = [n .. length $ head x]
  in if null lists
        then x : simplexHelper' (n + 1) xs
        else concat [choose num list' | num <- lists
                                      , list' <- x] :
             simplexHelper' (n + 1) xs

simplComp :: SimpMode
          -> Either QError ESMap
          -> Either [String] (GP.Gr () ESKey)
simplComp mode es =
  first ppError $
  liftM2 ((buildGr .) . flip (validSimplex mode)) keys es
  where keys = fmap M.keys es
        ppError (code,message) = lines $ message ++ "Error Code: " ++ show code

pp2DVec :: ESKey -> String
pp2DVec vec = res
  where res = (\[a,b] -> show a ++ " " ++ show b) $ toList vec

ppToFiles
  :: FilePath -> FilePath -> [(ESKey,ESKey)] -> IO ()
ppToFiles file1 file2 = writeFiles . process
  where process = (unlines *** unlines) . unzip . map (pp2DVec *** pp2DVec)
        writeFiles (a,b) =
          do writeFile file1 a
             writeFile file2 b

ppSimp
  :: Either [String] [(ESKey,ESKey)] -> Either [String] [String]
ppSimp = fmap ppPairs
  where ppPairs = map (\(x,y) -> show x ++ " " ++ show y)

-- keyTests
--   :: Int -> Either QError ESMap -> Either QError [[ESKey]]
-- keyTests simplexSize = fmap (choose simplexSize . M.keys)

buildEdgeSets
  :: Int -> Int -> [Vector Double] -> Either QError ESMap
buildEdgeSets nbdSize simplexSize data' = result
  where procPoints = processAllPts nbdSize simplexSize data'
        result = edgeSets data' procPoints
