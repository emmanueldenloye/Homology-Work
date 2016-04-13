module Combinations (choose) where

import           Control.Monad.Loops
import           Data.List

-- I stole this... tsk tsk
step' :: ([a], [a]) -> [([a], [a])]
step' (chosen, unchosen) = [(new : chosen, rest) | new:rest <- tails unchosen]

choose_ :: Int -> ([a], [a]) -> [([a], [a])]
choose_ n = concatM (replicate n step')

choose :: Int -> [a] -> [[a]]
choose n vs = fst <$> choose_ n ([], vs)
