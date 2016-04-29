{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Homology where

import Debug.Trace
import           Control.Arrow ((&&&),(***))
import           Control.Lens hiding (chosen, op)
import           Control.Monad.Loops
import           Data.Bifunctor()
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector.Storable as SV
import           Numeric.LinearAlgebra hiding (row)

data Entry =
  Entry {_row :: Integer
        ,_column :: Integer}
  deriving (Show)

makeLenses ''Entry

data SimplexEntry a = SimplexEntry
  { _entry :: Entry
  , _value :: a
  } deriving (Show)

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

data Sorted = Yes | No

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

homology :: (Ord a)
         => Int -> [[a]] -> Maybe Int
homology n xs = fmap (+ (rank'' + rank')) csize
  where (rank',csize) = (checkZero &&& checksz) $ incidenceMatrix n xs
        checksz sz =
          let (val,ret) = size sz
          in if val /= 0
                then Just ret
                else Nothing
        rank'' =
          if n < maxSize
             then 0
             else rank $
                  incidenceMatrix (n + 1)
                                  xs
        checkZero m =
          if n == 0
             then n
             else rank m
        maxSize = maximum . map length $ xs

-- CSR is the best representation to choose since most of the elements
-- are zero.
-- TODO: Need to find rank in an efficient way.
incidenceMatrix :: (Eq b,Ord b)
                => Int -> [[b]] -> Matrix Double
incidenceMatrix n xs = mat
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
        bdunzip = unzip . boundaryMapElem 1

cond1 :: (Eq b,Num b,Container c b)
      => c b -> c b -> c b -> Bool
cond1 x y z =
  any ((== 0) . sumElements)
      [x,y,z]

cond2 :: (Eq b,Num b,SV.Storable b)
      => [Vector b] -> Vector b -> Bool
cond2 prev cur =
  let f = SV.length . SV.takeWhile (== 0)
      g = ((== 1) . abs . SV.head . SV.dropWhile (== 0))
  in S.member (f cur)
              (S.fromList $ map f prev) &&
     g cur

-- CSR is the best representation to choose since most of the elements
-- are zero.
-- incidenceMatrixFast :: (Eq b,Ord b)
--                 => Int -> [[b]] -> CSR
incidenceMatrixFast
  :: (Num a, Ord b) => Int -> [[b]] -> [[SimplexEntry a]]
incidenceMatrixFast n xs =
  groupBy (on (==) (view $ entry . column)) $ convToSimplexEntry assocs'
  where simps = filter ((== n + 1) . length) xs
        -- mat = mkCSR assocs
        assocs' =
          concatMap (\x ->
                       uncurry (zipWith (\a b ->
                                           ((fromJust $ lookup b rassocs
                                            ,fromJust $ lookup x cassocs)
                                           ,a))) .
                       bdunzip $
                       x)
                    simps
        cassocs = zip simps [0 ..]
        rassocs = (`zip` [0 ..]) . nub . concatMap (snd . bdunzip) $ simps
        bdunzip = unzip . boundaryMapElem 1

boundaryMapElem :: (Num a,Ord b)
                => a -> [b] -> [(a,[b])]
boundaryMapElem s xs = zip (cycle [s,(-1) * s]) . map sort $ combs
  where combs = choose (length xs - 1) xs

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
  sortBy (comparing snd) . concatMap (uncurry boundaryMapElem)

boundaryOperator'' :: (Num a,Ord b)
                   => [(a,[b])] -> [(a,[b])]
boundaryOperator'' = concatMap (uncurry boundaryMapElem)

---- UTILITY/MISCELLANEOUS FUNCTIONS --

-- data ChoiceExpand = First | Second | ToDecide
--   -- This data type is used to guide the recursion in expandHelper.
--   -- The implied logic dictates which list should be expanded at a
--   -- particular point along the recursion.

-- What I was working on last night (Tue, 26, Apr, 2016, 01:12:41, EDT)
-- \f a as -> f <$> (foldr (\x acc -> ZipList x <*> acc) (ZipList a) -- as)
-- or \f a as -> f <$> (foldr ((<*>) . ZipList) (ZipList a) as)
-- It's type signature is :: Foldable t => (a -> b) -> [a] -> t [a -> a] -> ZipList b

-- buildRowChoices xs yss = map listFs yss
--   where listFs v = [map (* x) v | x <- [0,1,-1]]

-- expand :: (Num a,Ord a,Enum a)
--        => [SimplexEntry a]
--        -> [SimplexEntry a]
--        -> ([SimplexEntry a],[SimplexEntry a])
-- expand xs ys = expandHelper xs ys ToDecide

sv :: [((Integer,Integer),Integer)]
sv = [((0,1),1),((3,1),-1),((4,1),1)]

fv :: [((Integer,Integer),Integer)]
fv = [((0,0),1),((1,0),-1),((2,0),1)]

ffv :: [((Integer,Integer),Integer)]
ffv = [((0,0),1),((1,0),-1),((2,0),1),((5,0),1)]

ssv :: [((Integer,Integer),Integer)]
ssv = [((0,1),1),((3,1),-1),((4,1),1)]

data LinearOperation
  = Add
  | Sub
  | FlipSub
  | GetFirst
  | GetSecond
  | GetNegFirst
  | GetNegSecond
  deriving (Enum,Show)

type TestRows a = [[SimplexEntry a]]

type ValidRows a = [[SimplexEntry a]]

type Rank = Integer

data Form = Keep | Defer deriving Show

newtype Pivot = Pivot (Integer, Form) deriving Show

instance Eq Pivot where
  Pivot (n1, _) == Pivot (n2, _) = n1 == n2

instance Ord Pivot where
  Pivot (n1, _) `compare` Pivot (n2, _) = n1 `compare` n2

type Pivots = Set Pivot

newtype LinearOp a =
  LinearOp {runOp :: [a] -> [a]}

instance (Num a) => Monoid (LinearOp a) where
  mempty = LinearOp id
  mappend (LinearOp f) (LinearOp g) = LinearOp (g . f)

data RowComp a b =
  RowComp {comp :: a
          ,rankValue :: !b}
  deriving (Show)

rowElimination :: TestRows Integer -> (RowComp (ValidRows Integer, Pivots) Rank)
rowElimination = foldl' validateRow (RowComp ([],S.empty) 0)
  where validateRow cur@(RowComp (vs,ps) rank') testrow =
          case null vs of
            True -> RowComp (testrow : vs,pivotOnSimp testrow `S.insert` ps) 1
            False ->
              case (validateRowHelper testrow vs ps) of
                Just (validated,p) ->
                  RowComp (validated : vs,S.insert p ps)
                          (rank' + 1)
                Nothing -> cur

validateRowHelper :: (Show a,Num a, Eq a)
                  => [SimplexEntry a]
                  -> ValidRows a
                  -> Pivots
                  -> Maybe ([SimplexEntry a],Pivot)
validateRowHelper _ [] _ = Nothing -- This case shouldn't be reached.
validateRowHelper candidate valids pivots =
  fmap (backToSimp &&& pivot) . listToMaybe $
  dropWhile (\x -> testPivot x || testNotAnyZero x) rounds
  where round' [] _ [] = []
        round' (_:_) _ [] = error "You shouldn't get here."
        round' [] _ (_:_) = []
        round' (op:ops) c (s:ss) =
          runOp (mconcat . zipWith (flip intermediate) ss $ ops)
                (initTerm op c s)
        rounds =
          map (\xs -> round' xs candidate valids)
              (genOps (length valids))
        curRow = view (entry . column) $ head candidate
        backToSimp =
          filter ((/= 0) . (view value)) .
          zipWith (\c x ->
                     SimplexEntry (Entry c curRow)
                                  x)
                  [0 ..]
        -- This should be safe!
        initTerm op = on (linearOpTwoSimpList op) expand'
        intermediate op ys =
          LinearOp $
          linearOpOneSimpList op
                              (expand' ys)
        testNotAnyZero = all (== 0)
        testPivot x = S.member (pivot x) pivots

genOps :: Int -> [[LinearOperation]]
genOps n = map (take n) $ permutations [Add ..]

pivotOnSimp :: (Eq a, Num a)
            => [SimplexEntry a] -> Pivot
pivotOnSimp =
  Pivot .
  (toInteger . length *** form . listToMaybe) .
  span ((== 0) . (view value))
  where form x =
          case x of
            Just val ->
              let v = view value val
              in if abs v == 1
                    then Keep
                    else Defer
            Nothing -> Keep

pivot :: (Eq a,Num a)
      => [a] -> Pivot
pivot =
  Pivot .
  (toInteger . length *** form . listToMaybe) . span (== 0)
  where form x =
          case x of
            Just val ->
              if abs val == 1
                 then Keep
                 else Defer
            Nothing -> Keep

convToSimplexEntry
  :: [((Integer,Integer),a)] -> [SimplexEntry a]
convToSimplexEntry = map (\((r,c),v) -> SimplexEntry (Entry r c) v)

-- The first list should already be expanded before using this function.
linearOpOneSimpList
  :: Num c
  => LinearOperation -> [SimplexEntry c] -> [c] -> [c]
linearOpOneSimpList op [] xs = map ((getOp op) 0) xs
linearOpOneSimpList op ys [] =
  map (\y ->
         (getOp op) (view value y)
                    0)
      ys
linearOpOneSimpList op (y:ys) (x:xs) =
  (getOp op) (view value y)
              x :
  linearOpOneSimpList op ys xs

-- Both lists should already be expanded before using this function.
linearOpTwoSimpList
  :: Num c
  => LinearOperation -> [SimplexEntry c] -> [SimplexEntry c] -> [c]
linearOpTwoSimpList op' xs [] = map ((getOp op') 0 . (view value)) xs
linearOpTwoSimpList op' [] ys = map ((getOp op') 0 . (view value)) ys
linearOpTwoSimpList op' (x:xs) (y:ys) =
  on (getOp op')
     (view value)
     x
     y :
  linearOpTwoSimpList op' xs ys

getOp :: Num b => LinearOperation -> b -> b -> b
getOp op' =
  case op' of
    Add -> (+)
    Sub -> (-)
    FlipSub -> flip (-)
    GetFirst -> const
    GetSecond -> const id
    GetNegFirst -> ((* (-1)) .) . const
    GetNegSecond -> ((* (-1)) .) . const id

expand' :: Num a
        => [SimplexEntry a] -> [SimplexEntry a]
expand' [] = []
expand' v@[_] = v
expand' (x:y:xs) =
  if isSucc x y
     then x : y : expand' xs
     else let v = view (entry . column) x
          in [x] ++
             map (defaultEntry v)
                 [succ (view (entry . row) x) .. pred (view (entry . row) y)] ++
             [y] ++ expand' xs
  where isSucc =
          on ((==) . succ)
             (view (entry . row))

defaultEntry :: Num a
         => Integer -> Integer -> SimplexEntry a
defaultEntry c r =
  SimplexEntry (Entry r c)
               0

-- I don't think I am going to use this function any longer.
expandToEnd
   :: Num b
  => [SimplexEntry b] -> [SimplexEntry b] -> ([SimplexEntry b],[SimplexEntry b])
expandToEnd xs [] = (xs,[])
expandToEnd [] xs = ([],xs)
expandToEnd xss@(x:xs) yss@(y:ys) =
  let xr = view (entry . row) x
      xc = view (entry . column) x
      yr = view (entry . row) y
      yc = view (entry . column) y
  in case (null xs,null ys) of
       (True,True) ->
         case compare xr yr of
           GT ->
             (xs
             ,map (defaultEntry yc) $
              enumFromTo (succ xr)
                         yr)
           LT ->
             (map (defaultEntry xc) $
              enumFromTo xr
                         (pred yr)
             ,ys)
           EQ -> (xs,ys)
       (True,False) ->
         let yrr = view (entry . row) $ last ys
         in (x : (map (defaultEntry xc) $
              enumFromTo (succ xr)
                         yrr)
            ,yss)
       (False,True) ->
         let xrr = view (entry . row) $ last xs
         in (xss
            ,y : (map (defaultEntry yc) $
             enumFromTo (succ yr)
                        xrr))
       (False,False) ->
         bimap ((:) x)
               ((:) y) $
         expandToEnd xs ys
