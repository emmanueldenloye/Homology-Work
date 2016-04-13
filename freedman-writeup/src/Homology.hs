{-# LANGUAGE ScopedTypeVariables #-}
module Homology where

import           Control.Arrow
import           Control.Monad.Loops
import           Control.Monad.ST
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Debug.Trace
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

ex :: [[Integer]]
ex = [[],[0],[1],[2],[3],[0,1],[0,2],[1,2],[1,3],[2,3],[0,1,2]]

  -- This is listing is just meant to be one possible triangulation of
  -- a sphere.
sphere :: [[Integer]]
sphere =
  [[]
  ,[0]
  ,[1]
  ,[2]
  ,[3]
  ,[4]
  ,[0,1]
  ,[0,2]
  ,[0,3]
  ,[0,4]
  ,[1,2]
  ,[1,3]
  ,[2,3]
  ,[2,4]
  ,[0,1,2]
  ,[0,1,3]
  ,[0,2,3]
  ,[1,2,3]]

facets :: Eq a
       => [[a]] -> [[a]]
facets = catMaybes . go . sortBy (comparing length)
  where go [] = []
        go (x:xs) =
          if any (isSubsequenceOf x) xs
             then Nothing : go xs
             else Just x : go xs

step' :: ([a], [a]) -> [([a], [a])]
step' (chosen,unchosen) = [(new : chosen,rest) | new:rest <- tails unchosen]

choose_ :: Int -> ([a], [a]) -> [([a], [a])]
choose_ n = concatM (replicate n step')

choose :: Int -> [a] -> [[a]]
choose n vs = fst <$> choose_ n ([],vs)

-- homology :: (Ord a)
--          => Int -> [[a]] -> Int
-- homology n xs = csize - rank' - rank''
--   where (rank',csize) = (checkZero &&& snd . size) $ incidenceMatrix n xs
--         rank'' =
--           if n < maxSize
--              then 0
--              else rank $
--                   incidenceMatrix (n + 1)
--                                   xs
--         checkZero m =
--           if n == 0
--              then n
--              else rank m
--         maxSize = maximum . map length $ xs

-- CSR is the best representation to choose since most of the elements
-- are zero.
incidenceMatrix :: (Eq b,Ord b)
                => Int -> [[b]] -> CSR
incidenceMatrix n xs = mat
  where mat = mkCSR assocs
        simps = filter ((== n + 1) . length) xs
        assemb =
          map (\x -> uncurry (zip3 (repeat x)) . swap . bdunzip $ x) simps
        assocs =
          concatMap (map (\(a,b,c) ->
                            ((fromJust $ lookup b rassocs
                             ,fromJust $ lookup a cassocs)
                            ,c)))
                    assemb
        cassocs = zip simps [0 ..]
        rassocs = flip zip [0 ..] . nub . concatMap (snd . bdunzip) $ simps
        swap (a,b) = (b,a)
        bdunzip = unzip . boundaryMapElem 1

checkBoundaryOperator :: (Num a,Ord b)
                      => [[b]] -> a
checkBoundaryOperator = sum' . group' . boundaryOperator' . boundaryOperator
  where group' = groupBy ((==) `on` snd)
        sum' = sum . map (sum . map fst)

boundaryOperator :: (Num a,Ord b)
                 => [[b]] -> [(a,[b])]
boundaryOperator = boundaryOperator' . addCoeffs
  where addCoeffs = map (\x -> (1,x))

boundaryOperator' :: (Num a,Ord b)
                  => [(a,[b])] -> [(a,[b])]
boundaryOperator' =
  sortBy (comparing snd) . concatMap (uncurry boundaryMapElem)

boundaryOperator'' :: (Num a,Ord b)
                   => [(a,[b])] -> [(a,[b])]
boundaryOperator'' = concatMap (uncurry boundaryMapElem)

boundaryMapElem :: (Num a,Ord b)
                => a -> [b] -> [(a,[b])]
boundaryMapElem s xs = zip (cycle [s,(-1) * s]) . map sort $ combs
  where combs = choose (length xs - 1) xs
