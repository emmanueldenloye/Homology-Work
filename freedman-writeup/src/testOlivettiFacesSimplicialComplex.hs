{-# LANGUAGE BangPatterns #-}
module Main where


import           BuildSimplexes
import           Control.Arrow                  ((&&&), (***))
import           Control.DeepSeq
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lex.Fractional as BL
import           Data.List
import qualified Data.Vector.Storable           as SV
import           Data.Word
import           LocalRegionEdgeSet             (SimpMode (..))
import           Math.FFT
import           Numeric.LinearAlgebra
import           System.Environment

main :: IO ()
main = do [set',nbdPercent'] <- getArgs
          let filePrefix = "/home/emmanuel/Dropbox/haskell/data/umist_cropped"
              fileSuffix = ".dat"
              file = filePrefix ++ set' ++ fileSuffix
              sCount = lookup (read set' :: Int) olivettiCounts
              -- nbdPercent = let val = read nbdPercent' :: Double
              --                 in if val * snd sCount >= numPts
              --                       then floor $ fst sCount
          dataset <- fmap parseImages . B.readFile $ file
          let defvalue = Left ["You did not choose a valid file."]
              result = maybe defvalue
                        (\x ->
                            getOneSimpliciesMatrices nbdPercent
                                                     dataset
                                                     x
                                                     AndMode) sCount
              reshape' = join (***) (reshape 92)
              simplex = fmap (map reshape') result
          print $ fmap head simplex

olivettiCounts :: [(Int,(Int,Int))]
olivettiCounts =
  [(0,(38,10304))
  ,(1,(35,10304))
  ,(2,(26,10304))
  ,(3,(24,10304))
  ,(4,(26,10304))
  ,(5,(23,10304))
  ,(6,(19,10304))
  ,(7,(22,10304))
  ,(8,(20,10304))
  ,(9,(32,10304))
  ,(10,(34,10304))
  ,(11,(34,10304))
  ,(12,(26,10304))
  ,(13,(30,10304))
  ,(14,(19,10304))
  ,(15,(26,10304))
  ,(16,(26,10304))
  ,(17,(33,10304))
  ,(18,(48,10304))
  ,(19,(34,10304))]

parseImages :: B.ByteString -> [Matrix Double]
parseImages =
  unfoldr (\v ->
             if B.null v
                then Nothing
                else Just . bimap parseLine B.tail . B.span (/= 10) $ v)

parseLine :: B.ByteString -> Matrix Double
parseLine = reshape 92 . SV.unfoldr step
  where
    step !s = proc' =<< B.uncons s
    proc' (x,xs)
      | x == (32 :: Word8) =
          case BL.readDecimal xs of
              Nothing -> Nothing
              Just (!k,!t) -> Just (k, B.tail t)
      | otherwise =
          case BL.readDecimal (B.cons x xs) of
              Nothing -> Nothing
              Just (!k,!t) ->
                  if B.null t
                      then Just (k, B.empty)
                      else Just (k, B.tail t)
