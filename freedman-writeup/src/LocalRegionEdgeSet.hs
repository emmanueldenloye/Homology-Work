{-# LANGUAGE TupleSections #-}

module LocalRegionEdgeSet
       (edgeSet, edgeSets, SimpMode(..), QError, ESKey,
        ESValue, ESMap, validSimplex)
       where

import           Control.Arrow                     ((&&&))
import           Control.Monad
import           Data.Either
import           Data.Foldable                     hiding (toList)
import           Data.Graph.Inductive              hiding (ap)
import qualified Data.Graph.Inductive.PatriciaTree as GP
import           Data.List
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe
import           Numeric.LinearAlgebra             hiding (find, (<>))
import           Numeric.Qhull
import           Prelude                           hiding (concat)
import           TangentSpaces

type QError = (Int,String)
type ESKey = Vector Double
type ESValue = [Vector Double]
type ESAssoc = (ESKey,ESValue)
type ESMap = Map ESKey ESValue

-- TODO: I could replace the foldl' with foldM instead.
edgeSets
  :: [Vector Double] -> [PType] -> Either QError ESMap
edgeSets m xs = fmap M.fromList query
  where query = validElems $ map (`edgeSet` m) xs
        validElems :: [Either QError (Maybe ESAssoc)]
                   -> Either QError [ESAssoc]
        validElems = foldl' build' $ Right []
          where build' (Right r) e =
                  case e of
                    Right x ->
                      case x of
                        Just y -> Right (y : r)
                        Nothing -> Right r
                    Left invalid -> Left invalid
                build' x@(Left _) _ = x

data SimpMode
  = AndMode
  | OrMode

validSimplex
  :: SimpMode -> ESMap -> [ESKey] -> [Context () ESKey]
validSimplex smode m =
  unfoldr (\v ->
             if null v
                then Nothing
                else let (keyVal,keyInd) = head v
                         rest = tail v
                     in Just (toContext keyInd $
                              mapMaybe (pairInComplex smode m keyVal) rest
                             ,rest)) .
  (`zip` [0 :: Int ..])
  where toContext a b = (b,a,(),b)
        pairInComplex inSmode esMap keyA val@(keyB,_) =
          case inSmode of
            AndMode ->
              if uncurry all predList
                 then Just val
                 else Nothing
            OrMode ->
              if uncurry any predList
                 then Just val
                 else Nothing
          where valueA = M.lookup keyA esMap
                valueB = M.lookup keyB esMap
                aInb = lookup' keyA valueB
                bIna = lookup' keyB valueA
                lookup' k = (=<<) (find (== k))
                predList = (isJust,[aInb,bIna])

edgeSet
  :: PType -> [Vector Double] -> Either QError (Maybe ESAssoc)
edgeSet Boundary _ = Right Nothing
edgeSet (HasATS basis point) points' = fromC . uncurry qhull  . mapMaybe' $ points'
  where pointInd = fromJust $ elemIndex point points' -- This should be safe!
        alpha y = point - y
        beta y = 0.5 * (y - point)
        gamma y = basis #> alpha y
        delta y = alpha y `dot` beta y
        mapMaybe' = (size . head &&& vjoin) . mapMaybe epsilon
        epsilon y =
          if point == y
             then Nothing
             else Just . cmap (/ (delta y + peps)) $ gamma y
        fromC =
          either (Left . (errorCode &&& errorMessage))
                 (Right . Just . (,) point . getHull' . nub . toList . vjoin)
        getHull' inds =
          map fst . filter (\(e,i) -> elem i (correct pointInd) && e /= point) $
          zip points' [0 ..]
          where correct indp =
                  map (\i ->
                         if i < indp
                            then i
                            else i + 1)
                      inds
