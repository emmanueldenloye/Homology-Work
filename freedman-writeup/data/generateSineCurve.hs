import System.Environment
import System.Random
import Data.List

main :: IO ()
main = do
    [numPts'] <- getArgs
    let numPts = read numPts'
        points = linSpace 0 (2*pi) numPts
        sineCurve = zip <*> map sin $ points
        datToFile = unlines . map (\(a,b) -> show a ++ " " ++ show b) $ sineCurve
    writeFile ("sineCurve" ++ "numPts" ++ numPts' ++ ".txt") datToFile

linSpace :: Double -> Double -> Int -> [Double]
linSpace a b n' = let inc = (b - a) / n
                      n = fromIntegral n'
                  in map (\x -> a + inc * x) [0..n-1]
