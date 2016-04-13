import System.Environment
import Control.Arrow ((***))
import Control.Monad (join)

data Plane = XY | XZ | YZ deriving Show

torus :: (Double,Double,Double,Double,Double)
        -> Plane
        -> [Double]
        -> [[Double]]
torus (iR, oR, x, y, z) plane angle =
  case plane of
    XY ->
      let (Just xpts, Just ypts) = make' (Just oR, Just iR) angle
          (Just zpts, Nothing) = make' (Just iR, Nothing) angle
      in collect xpts ypts zpts
    XZ ->
      let (Just xpts, Just zpts) = make' (Just oR, Just iR) angle
          (Just ypts, Nothing) = make' (Just iR, Nothing) angle
      in collect xpts ypts zpts
    YZ ->
      let (Just ypts, Just zpts) = make' (Just oR, Just iR) angle
          (Just xpts, Nothing) = make' (Just iR, Nothing) angle
      in collect xpts ypts zpts
  where
    collect = zipWith3 (\a b c -> [x+a,y+b,z+c])
    make' pair as = (join (***)) sequence $ unzip [makepts pair a1 a2 | a1 <- as, a2 <- as]
    makepts (mr1, mr2) ang1 ang2 =
      case (mr1, mr2) of
        (Just r1, Just r2) -> (Just $ (r1 +  r2*(cos ang1))*(cos ang2), Just $ (r1 + r2*(cos ang1))*(sin ang2))
        (Just r, Nothing) -> (Just $ r * sin ang1, Nothing)
        _                 -> error "This should be an impossible case to enter!"

linearScale :: Int -> Double -> Double -> [Double]
linearScale n a b = map (\i -> a + i * inc') [0.. fromIntegral $ n-1]
  where
    inc' = (b - a) / fromIntegral n

main :: IO ()
main = do
  [numPts'] <- getArgs
  let val1 = (1,5,0,0,0)
      angles = linearScale numPts 0 (2*pi)
      numPts = read numPts' :: Int
      fileName = "torus" ++ "Plane" ++ (show orientation) ++ (show $ numPts * numPts) ++ "points" ++ rest val1 ++  ".txt"
      rest (o,r,x,y,z) = "outer" ++ show o ++ "inner" ++ show r ++ "XC" ++ show x ++ "YC" ++ show y ++ "ZC" ++ show z
      orientation = XY
      torusData = torus val1 orientation angles
      ppPrint = unlines . map (unwords . map show)

  writeFile fileName (ppPrint torusData)
