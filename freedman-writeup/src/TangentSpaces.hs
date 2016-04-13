{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module TangentSpaces (processAllPts, PType(..)) where

import           Combinations
import           Control.Arrow hiding (first, second)
import           Control.Monad
import           Control.Monad.ST
import           Data.Bifunctor
import           Data.Function
import           Data.List (sortBy, (\\))
import           Data.Maybe
import           Data.Monoid hiding ((<>), All(..))
import           Data.Ord
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

data PType =
             HasATS
               { matrix' :: {-# UNPACK #-} !(Matrix Double)
               , point' :: {-# UNPACK #-} !(Vector Double)
               }
           | Boundary
  deriving (Eq, Show)

processAllPts :: Int -- | Number of points in a neighborhood
              -> Int -- | Number of points to include in enclosure test
              -> [Vector Double] -- | This matrix's rows contain the dataset's points.
              -> [PType]
processAllPts numPts encNum points' =
  map (determineATS encNum <*> funcNearest) points'
  where
    funcNearest v = theNearest numPts v (fromRows points')

determineATS :: Int -> Vector Double -> Matrix Double -> PType
determineATS n point m = ats . dropWhile isNothing . procProjs $ getKNNs
  where
    ats (Just m':_) = HasATS m' point
    ats _ = Boundary
    otherpts = on (\\) toRows
    procProjs = map (findProjCoeffsATS point <*> otherpts m)
    getKNNs = computeKNNs n m

findProjCoeffsATS :: Vector Double
                  -> Matrix Double
                  -> [Vector Double]
                  -> Maybe (Matrix Double)
findProjCoeffsATS point mat nbd = maybe Nothing test t1
  where
    allpts = point : nbd
    (t1:t2) = map (`enclosureTest` mat) allpts
    test a =
      if isJust . getFirst $ foldMap First t2
        then Nothing
        else Just a

enclosureTest :: Vector Double -> Matrix Double -> Maybe (Matrix Double)
enclosureTest tvec mat =
  if fromMaybe False $ testATS <$> theta
    then res
    else Nothing
  where
    psiVec = asColumn $ formPsiVector (fst ctr) xº
    ctr = centerWithFirstRow mat
    (mat', xº) = formOmegaMatrix `bimap` (tvec -) $ ctr
    theta = vecize <$> linearSolve mat' psiVec
    sVec = scalar . (1 -) . sumElements
    vecize = vjoin . uncurry (:) . (sVec &&& (: [])) . flatten
    testATS =
      all (\v -> v >= 0 && v <= 1) .
      toList
    res = Just $ fst ctr

theNearest :: Int -> Vector Double -> Matrix Double -> Matrix Double
theNearest n point mat = (??) mat (Pos idists, All)
  where
    idists = sortAndDump . fromList . map (join (<.>)) $ toRows centered
    sortAndDump = subVector 0 n . sortIndex
    centered =
      let (rr, _) = size mat
      in runST $
        do
          m <- thawMatrix mat
          forM_ [0 .. rr - 1] $ centerMatrixRow m $ asRow point
          freezeMatrix m

computeKNNs :: Int -> Matrix Double -> [Matrix Double]
computeKNNs n = map (fromRows . map fst) . process
  where
    sorting = sortBy (comparing $ map snd) . map (sortBy $ comparing snd)
    collectDists = zip <*> map (join (<.>))
    process = sorting . choose n . collectDists . toRows

centerMatrixRow :: forall s t. (Num t, Num (Vector t), Container Vector t)
                            => STMatrix s t -> Matrix t -> Int -> ST s ()
centerMatrixRow m v r = do
  row' <- extractMatrix m (Row r) AllCols
  setMatrix m r 0 $ row' - v

centerWithFirstRow :: Matrix Double -> (Matrix Double, Vector Double)
centerWithFirstRow mat =
  let (rr, _) = first (1 -) . size $ mat
      (xº, mat') = (takeRows 1 &&& dropRows 1) mat
      res = flip (,) $ flatten xº
  in res $
    runST $
      do
        m <- thawMatrix mat'
        forM_ [0 .. rr - 1] $ centerMatrixRow m xº
        freezeMatrix m

formOmegaMatrix :: Matrix Double -> Matrix Double
formOmegaMatrix mat = mat <> tr' mat

formPsiVector :: Matrix Double -> Vector Double -> Vector Double
formPsiVector mat pt = (* 0.5) $ mat #> pt + pt <# tr' mat
