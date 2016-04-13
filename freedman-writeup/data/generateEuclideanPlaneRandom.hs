import System.Environment
import System.Random
import Data.List

main :: IO ()
main = do
    [d',numPts'] <- getArgs
    let numPts = read numPts' :: Int
        d = read d' :: Int
    points <- chop d . take (d * numPts) . randomRs (-10 :: Double,10) <$> newStdGen
    let datToFile = unlines . map (unwords . map show) $ points
    writeFile ("euclideanRandomDIM" ++ d' ++ "numPts" ++ numPts' ++ ".txt") datToFile

chop :: Int -> [a] -> [[a]]
chop n = unfoldr (\v -> if null v then Nothing else Just $ splitAt n v)
