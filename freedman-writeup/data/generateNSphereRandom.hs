import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.List
import System.Environment
import System.Random
import Numeric.LinearAlgebra (peps)

main :: IO ()
main = do
    [d',numPts'] <- getArgs
    let numPts = read numPts' :: Int
        d = read d' :: Int
    points <- nub . map unitVec . chop d . take (d * 3 * numPts) . randomRs (-100 :: Double,100) <$> newStdGen
    -- I am just being lazy here by oversampling `(d * 3 * numPts)` and hoping to get more than 'numPts' points.
    let datToFile = unlines . map (unwords . map show) $ points
    writeFile ("SphereRandomDIM" ++ d' ++ "numPts" ++ numPts' ++ ".txt") datToFile

chop :: Int -> [a] -> [[a]]
chop n = unfoldr (\v -> if null v then Nothing else Just $ splitAt n v)

unitVec :: (Num a, RealFloat a, Fractional a) => [a] -> [a]
unitVec =  uncurry map . (flip (/) . selfdot &&& id)
  where
    selfdot = foldl' (+) peps . join (zipWith (*))
