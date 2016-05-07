{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Homology where

import           Control.Arrow
import           Control.Lens          hiding (chosen, op)
import           Control.Monad.Loops
import           Data.Function
import           Data.List
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Data.Ord
import           Debug.Trace
import           Numeric.LinearAlgebra hiding (find, row)

data Entry =
  Entry {_row    :: !Integer
        ,_column :: !Integer}
  deriving (Show)

makeLenses ''Entry

data SimplexEntry = SimplexEntry
  { _entry :: !Entry
  , _value :: !Integer
   } deriving Show

makeLenses ''SimplexEntry

ex :: [[Integer]]
ex = [[],[0],[1],[2],[3],[0,1],[0,2],[1,2],[1,3],[2,3],[0,1,2]]

-- This listing is just meant to be one possible triangulation of a
-- sphere.
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

data Sorted
  = Yes
  | No

facets :: Eq a
       => Sorted -> [[a]] -> [[a]]
facets sort' =
  let func = catMaybes . go
  in case sort' of
       No -> func . sortBy (comparing length)
       Yes -> func
  where go [] = []
        go (x:xs) =
          if any (isSubsequenceOf x) xs
             then Nothing : go xs
             else Just x : go xs

step' :: ([a],[a]) -> [([a],[a])]
step' (chosen,unchosen) = [(new : chosen,rest) | new:rest <- tails unchosen]

choose_ :: Int -> ([a],[a]) -> [([a],[a])]
choose_ n = concatM (replicate n step')

choose :: Int -> [a] -> [[a]]
choose n vs = fst <$> choose_ n ([],vs)

homologyQuickView :: (Ord a)
         => Int -> [[a]] -> Maybe Int
homologyQuickView n xs = fmap (+ (rank'' + rank')) csize
  where (rank',csize) = (checkZero &&& checksz) $ incidenceMatrixQuickView n xs
        checksz sz =
          let (val,ret) = size sz
          in if val /= 0
                then Just ret
                else Nothing
        rank'' =
          if n < maxSize
             then 0
             else rank $
                  incidenceMatrixQuickView (n + 1)
                                           xs
        checkZero m =
          if n == 0
             then n
             else rank m
        maxSize = maximum . map length $ xs

-- CSR is the best representation to choose since most of the elements
-- are zero.
-- TODO: Need to find rank in an efficient way.
incidenceMatrixQuickView :: (Eq b,Ord b)
                => Int -> [[b]] -> Matrix Double
incidenceMatrixQuickView n xs = mat
  where mat = assoc (rlen,clen) 0 assocs
        simps = filter ((== n + 1) . length) xs
        assemb =
          map (\x -> uncurry (zip3 (repeat x)) . swap . bdunzip $ x) simps
        assocs =
          concatMap (map (\(a,b,c) ->
                            ((fromJust $ lookup b rassocs
                             ,fromJust $ lookup a cassocs)
                            ,c)))
                    assemb
        (clen,cassocs) = (length &&& id) . zip simps $ [0 ..]
        (rlen,rassocs) =
          (length &&& id) . flip zip [0 ..] . nub . concatMap (snd . bdunzip) $
          simps
        swap (a,b) = (b,a)
        bdunzip = unzip . boundaryMapElem' 1

sortOnRow
  :: [[SimplexEntry]] -> [[SimplexEntry]]
sortOnRow = map (sortOn (view (entry . row)))

incidenceMatrixFast
  :: Int -> [[Integer]] -> [[SimplexEntry]]
incidenceMatrixFast n xs = map (uncurry $ boundaryMapElem 1) listed
  where focus =
          takeWhile ((== n + 1) . length) . dropWhile ((< n + 1) . length) $ xs
        listed = zip focus [0 ..]

homologyFast
  :: Int -> [[Integer]] -> ValidColumns
homologyFast n = comp . columnElimination . incidenceMatrixFast n

boundaryMapElem' :: (Num a,Ord b)
                => a -> [b] -> [(a,[b])]
boundaryMapElem' s xs = zip (cycle [s,(-1) * s]) . map sort $ combs
  where combs = choose (length xs - 1) xs

boundaryMapElem
  :: Integer -> [Integer] -> Integer -> [SimplexEntry]
boundaryMapElem s xs n =
  zipWith formCell (cycle [s,(-1) * s]) . map ((,) n . getIndex) $ combs
  where combs = choose (length xs - 1) xs
        getIndex [] = 0 -- This is needed for the zeroth-homology case.
        getIndex [a] = a
        getIndex a@(_:_) = pred $ sum a
        formCell value' (column',row') =
          SimplexEntry (Entry row' column')
                       value'

type TestColumns = [[SimplexEntry]]

-- A Map is useful here for O(log n) search of the rows.
type ValidColumns = Map Pivot [SimplexEntry]

type Rank = Integer

type Sign = Integer

type Extent = Integer

data Form
  = Keep
  | Defer
  deriving (Eq,Show)

newtype Pivot =
  Pivot (Extent,Form,Sign)
  deriving (Show,Eq)

instance Ord Pivot where
  Pivot (e1,_,s1) `compare` Pivot (e2,_,s2) =
    mappend (e1 `compare` e2)
            (s1 `compare` s2)

data ColumnComp =
  ColumnComp {comp      :: ValidColumns
             ,rankValue :: !Integer}
  deriving (Show)

columnElimination
  :: TestColumns -> ColumnComp
columnElimination = foldl' validateColumn (ColumnComp M.empty 0)
  where validateColumn cur@(ColumnComp vmap rank') testColumn =
          if M.null vmap
             then ColumnComp (M.insert (pivotOnSimp testColumn) testColumn vmap) 1
             else case validateHelper testColumn vmap of
                    Just (validated,p) ->
                      ColumnComp (M.insert p validated vmap)
                              (rank' + 1)
                    Nothing -> cur

validateHelper
  :: [SimplexEntry]
  -> ValidColumns
  -> Maybe ([SimplexEntry],Pivot)
validateHelper candidate valids
  | M.null valids = Nothing
  | otherwise =
    do result@(opVal,start) <- (cleanColumn &&& pivotOnSimp) <$> operation
       if null opVal
          then Nothing
          else case matchingPivot' start of
                 Just _ -> validateHelper opVal valids
                 Nothing -> Just result
  where cleanColumn = filter ((/= 0) . view value)
        pivotOfCand = pivotOnSimp candidate
        operation =
          let v = fromJust $ fst <$> matchCand
              matchCand = matchingPivot' pivotOfCand
              comparison =
                ((,second' pivotOfCand) . compare pivotOfCand) . snd <$>
                matchCand
          in case comparison of
               Just (EQ,_) -> Just $ on (specialZip (-)) expand' candidate v
               Just (_,Keep) -> Just $ on (specialZip (+)) expand' candidate v
               Just (GT,Defer) -> Nothing
               Just (LT,Defer) -> Just candidate
               Nothing -> Just candidate
        second' (Pivot (_,a,_)) = a
        specialZip _ [] [] = []
        specialZip _ xs [] = xs
        specialZip _ [] xs = xs
        specialZip f (x:xs) (y:ys) = buildCell f x y : specialZip f xs ys
        buildCell f a b =
          SimplexEntry {_entry =
                          Entry {_row = view (entry . row) a
                                ,_column = view (entry . column) a}
                       ,_value = on f (view value) a b}
        matchingPivot' a =
          fmap ((,) <*> pivotOnSimp) . find ((== a) . pivotOnSimp) $ valids

pivotOnSimp :: [SimplexEntry] -> Pivot
pivotOnSimp = form . listToMaybe . dropWhile ((== 0) . view value)
  where form x =
          case x of
            Just val ->
              let v = view value val
                  r = view (entry . row) val
                  sign = signum . view value $ val
              in if abs v == 1
                    then Pivot (r,Keep,sign)
                    else Pivot (r,Defer,sign)
            Nothing -> Pivot (0,Keep,0)

expand' :: [SimplexEntry] -> [SimplexEntry]
expand' [] = []
expand' v@[_] = v
expand' (x:y:xs) =
  if isSucc x y
     then x : expand' (y : xs)
     else let v = view (entry . column) x
          in [x] ++
             map (defaultEntry v)
                 [succ (view (entry . row) x) .. pred (view (entry . row) y)] ++
             expand' (y : xs)
  where isSucc =
          on ((==) . succ)
             (view (entry . row))

convToSimplexEntry
  :: (Integral a) => [((Integer,Integer),a)] -> [SimplexEntry]
convToSimplexEntry =
  map (\((r,c),v) ->
         SimplexEntry (Entry r c)
                      (toInteger v))

defaultEntry
  :: Integer -> Integer -> SimplexEntry
defaultEntry c r =
  SimplexEntry (Entry r c)
               0

---- UTILITY/MISCELLANEOUS FUNCTIONS ----

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
  sortBy (comparing snd) . concatMap (uncurry boundaryMapElem')

boundaryOperator'' :: (Num a,Ord b)
                   => [(a,[b])] -> [(a,[b])]
boundaryOperator'' = concatMap (uncurry boundaryMapElem')

---- UTILITY/MISCELLANEOUS FUNCTIONS --
