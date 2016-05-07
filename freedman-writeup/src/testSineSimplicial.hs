import           BuildSimplexes
import           Homology
import           LocalRegionEdgeSet
import           Numeric.LinearAlgebra (loadMatrix)
import           System.Environment

main :: IO ()
main =
  do [nbdFactor',kdims',groupNum'] <- getArgs
     let file =
           "/home/emmanuel/Dropbox/haskell/homologyWork/" ++
           "freedman-writeup/data/SphereRandomDIM3numPts100.txt"
         kdims = read kdims' :: Int
         nbdFactor = read nbdFactor' :: Double
     dataset <- loadMatrix file
     let result = getOneSimplicies nbdFactor kdims dataset AndMode
         allSimplicies = getAllSimplicies kdims <$> result
         columnComputation =
           fmap (columnElimination .
                 incidenceMatrixFast (read groupNum' :: Int))
                allSimplicies
     print $ rankValue <$> columnComputation

 -- With arguments 1.4 3 2, I had to stop the simulation at 93741.88
 -- seconds or 26 hours. Yeesh! Memory usage is not the issue. Map access is!
