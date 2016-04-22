import           BuildSimplexes
import           Data.Graph.Inductive  (scc)
import           Data.List
import           Homology
import           LocalRegionEdgeSet
import           Numeric.LinearAlgebra
import           System.Environment

data Pair =
  Pair !Int
       !Int
  deriving (Show)

-- (/ (* 295 294 293) (* 3 2)) 4235315 (This is huge!)

main :: IO ()
main =
  do [nbdFactor',kdims'] <- getArgs
     let file =
           "/home/emmanuel/Dropbox/haskell/homologyWork/" ++
           "freedman-writeup/data/SphereRandomDIM3numPts300.txt"
         kdims = read kdims' :: Int
         nbdFactor = read nbdFactor' :: Double
     dataset <- loadMatrix file
     let result = getOneSimplicies nbdFactor kdims dataset AndMode
     -- print $ (nub . map length . scc) <$> result
     print $
       fmap (getAllSimplicies kdims) result

-- foldl' (\(Pair acc1 acc2) ((x1,x2),_) ->
--                        Pair (max acc1 x1)
--                             (max acc2 x2))
--                     (Pair 0 0) .
