-- import           Control.Monad
-- import           Data.IORef
-- import           TangentSpaces
import           LocalRegionEdgeSet
import           Numeric.LinearAlgebra
import           System.Environment

main :: IO ()
main = do
  [k1] <- getArgs
  let file = "/home/emmanuel/Dropbox/haskell/brun_fast_git/data/swissroll.dat"
  -- let file = "/home/emmanuel/Dropbox/haskell/brun_fast_git/data/torusData5k.txt" let file =
  -- "/home/emmanuel/Dropbox/haskell/freedman/data/euclideanRandomDIM2numPts1000.txt"
  points <- loadMatrix file

  let (_, dims) = size points
      numPts = floor $ fromIntegral dims * (read k1 :: Double)
      rows' = toRows points
      -- ats = processAllPts numPts (dims + 1) rows'

  -- let edgeS = ((\m -> map (`edgeSet` m)) <*> processAllPts numPts (dims + 1)) rows'

  -- print edgeS
  let es = edgeSets numPts  (dims + 1) rows'
  print $ buildValidEdgeSet es

  -- This code is  for testing the generation of the approximating tangent spaces

  -- cnt <- newIORef (0 :: Int)
  -- mapM_
  --   (\v -> do
  --      print v
  --      let test =
  --            case v of
  --              HasATS _ _ -> True
  --              Boundary -> False
  --      when test (modifyIORef' cnt (+ 1)))
  --   ats
  -- readIORef cnt >>= print
  -- print n
