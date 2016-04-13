{-# LANGUAGE BangPatterns #-}

module Main where

import           BuildSimplexes
import           Control.Arrow                  ((&&&), (***))
import           Control.DeepSeq
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lex.Fractional as BL
import           Data.Function
import           Data.Graph.Inductive
import           Data.List
import qualified Data.Vector.Storable           as SV
import           Data.Word
import           Homology
import           LocalRegionEdgeSet             (SimpMode (..))
import           Math.FFT
import           Numeric.LinearAlgebra
import           System.Environment

main :: IO ()
main =
  do [digit',nbdFactor',kdims'] <- getArgs
     let file = "/home/emmanuel/Dropbox/haskell/brun_fast_git/data/mnist.txt"
         digit = read digit' :: Int
         kdims = read kdims' :: Int
         nbdFactor = read nbdFactor' :: Double
     dataset <-
       fmap (parseDigit (read digit' :: Double) . parseImageAndID) . B.readFile $
       file
     let result =
           if digit <= 9 && digit >= 0
              then getOneSimpliciesMatrices nbdFactor kdims dataset AndMode
              else defvalue
         defvalue =
           Left ["You did not pick a digit represented " ++
                 "in the MNIST dataset. Choose an integer " ++
                 "i, such that 0 <= i <= 9. "]
     print $ fmap (incidenceMatrix 4 . getAllSimplicies) result
     -- print $ fmap (incidenceMatrix 3 . group) result

-- reshape' = join (***) (reshape 28)
-- simplex = fmap (map reshape') result
-- !() = deepseq simplex ()
-- mnistCounts :: [(Int, Int)]
-- mnistCounts =
--     [ (0, 479)
--     , (1, 563)
--     , (2, 488)
--     , (3, 493)
--     , (4, 535)
--     , (5, 434)
--     , (6, 501)
--     , (7, 550)
--     , (8, 462)
--     , (9, 495)]

parseDigit
    :: (Eq b)
    => b -> [(a, b)] -> [a]
parseDigit digit = map fst . filter ((== digit) . snd)

parseImageAndID :: B.ByteString -> [(Matrix Double, Double)]
parseImageAndID =
    unfoldr
        (\v ->
              if B.null v
                  then Nothing
                  else Just . bimap parseLine B.tail . B.span (/= 10) $ v)

parseLine :: B.ByteString -> (Matrix Double, Double)
parseLine = (reshape 28 . SV.init &&& SV.last) . SV.unfoldr step'
  where
    step' !s = proc' =<< B.uncons s
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
