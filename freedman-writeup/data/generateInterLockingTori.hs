import           Control.Arrow                           ((***))
import           Control.Monad                           (join, void)
import           Graphics.Gnuplot.Advanced               as GP
import           Graphics.Gnuplot.Frame                  as Frame
import           Graphics.Gnuplot.Frame.OptionSet        as Opts
import           Graphics.Gnuplot.Graph                  as Graph
import           Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import           Graphics.Gnuplot.Plot.ThreeDimensional  as Plot3D hiding (linearScale)
import           System.Environment

data Plane = XY | XZ | YZ deriving Show

-- I'M GOING TO NEED TO USE ANOTHER LIBRARY. THIS ONE IS NOT VERY RELIABLE FOR SOME ODD REASON.

main :: IO ()
main = do
  [numPts1',numPts2'] <- getArgs
  let val1 = (5,0.1,0,0,0)
      val2 = (5,0.1,0,10,0)
      angles1 = linearScale numPts1 0 (2*pi)
      angles2 = linearScale numPts2 0 (2*pi)
      orient1 = XY
      orient2 = YZ
      (torus1, torus2) = (torus val1 orient1 angles1, torus val2 orient2 angles2)
      numPts1 = read numPts1' :: Int
      numPts2 = read numPts2' :: Int
      ppPrint = unlines . map (unwords . map show)
      fileName = "tori" ++ info ++ ".txt"
      info = numInfo ++ oinfo ++ rest (val1,val2)
      oinfo = "o1" ++ (show orient1) ++ "o2" ++ (show orient2)
      numInfo = show (numPts1 * numPts1 + numPts2 * numPts2)
      rest (t1,t2) = "t1" ++ rest' t1 ++ "t2" ++ rest' t2
      rest' (o,r,x,y,z) = "outer" ++ show o ++ "inner" ++ show r ++ "XC" ++ show x ++ "YC" ++ show y ++ "ZC" ++ show z
      -- THIS MANY LETS IN AN IO BLOCK SHOULD BE A CRIME.

  mes <- GP.plotDefault $ interlock torus1 torus2

  print mes -- I NEED TO PRINT THE EXIT CODE FOR SOME STRANGE REASON. wAt?

  writeFile fileName (ppPrint $ torus1 ++ torus2)

linearScale :: Int -> Double -> Double -> [Double]
linearScale n a b = map (\i -> a + i * inc') [0.. fromIntegral $ n-1]
  where
    inc' = (b - a) / fromIntegral n

defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
  Opts.key False Opts.deflt

interlock :: [[Double]] -> [[Double]] -> Frame.T (Graph3D.T Double Double Double)
interlock ps1 ps2 =
  let formatTriplet [a,b,c] = (a,b,c)
  in Frame.cons
      (Opts.grid True $
       Opts.view 42 17 1 1 $
       Opts.xRange3d (-50, 50) $
       Opts.yRange3d (-50, 50) defltOpts) $
      Plot3D.cloud Graph3D.points (map formatTriplet ps1)
      `mappend`
      Plot3D.cloud Graph3D.points (map formatTriplet ps2)

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
