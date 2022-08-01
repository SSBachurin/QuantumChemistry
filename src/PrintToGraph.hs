module PrintToGraph where
import           CommonPhysics
import           HydrogenFunctions
import           Mathematics

import qualified Graphics.Gnuplot.Advanced               as GP

import qualified Graphics.Gnuplot.Frame                  as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet        as OptsSet


import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional  as Plot3D

import           GHC.Exts                                (groupWith)
import qualified Graphics.Gnuplot.LineSpecification      as LineSpec


import           Data.Foldable                           (Foldable (foldMap'))
import           Data.List                               (elemIndex)
import           Data.Maybe                              (fromJust)
import           GHC.IO.Exception                        (ExitCode)
import           Graphics.Gnuplot.ColorSpecification     (paletteFrac)
import qualified Graphics.Gnuplot.Value.Atom             as Atom


-- printGraph :: ((Double, Double, Double)->Double) -> IO ()
-- printGraph func = do

printToGraph :: IO ()
printToGraph = do
    putStrLn "write function for which you need to build a graph"
    input <- getLine
    let func = handlerFunctions input
    sequence_ [GP.plotDefault (waveFuncVis func 10000 0.0005)]
    print "Done"

defltOpts :: OptsSet.T (Graph3D.T Double Double Double)
defltOpts = OptsSet.key False  OptsSet.deflt

waveFuncVis :: ((Double, Double, Double) ->  Double) -> Double -> Double -> Frame.T (Graph3D.T Double Double Double)
waveFuncVis func depth precision =
      let x = Plot3D.linearScale 100 (-15, 15)
          testedRange = (groupWith (\(x,y,z) -> test func (x,y,z) depth precision)  . filter (\(x,y,z) -> funcWrapper func x y z^2 >= precision)) [(x1,y1,z1) | x1<-x, y1<-x, z1<-x]
          range = [(x1,y1,z1) | x1<-x, y1<-x, z1<-x]
          calcColor :: [(Double,Double,Double)] -> Double
          calcColor array  =  fromIntegral (fromJust (elemIndex array testedRange)) / fromIntegral (length testedRange)
          linespec array   = Graph3D.lineSpec $ LineSpec.lineColor (paletteFrac (calcColor array)) LineSpec.deflt
          graph array  = linespec array <$> Plot3D.cloud Graph3D.points array
    in  Frame.cons defltOpts $ foldMap' graph testedRange

test :: ((Double, Double, Double) -> Double)
    -> (Double, Double, Double) -> Double -> Double -> Integer
test func (x, y , z) depth precision
    |  funcWrapper func x y z^2 >= precision = round $ funcWrapper func x y z^2 * depth
    |  otherwise = 0

funcWrapper :: ((Double, Double, Double) ->  Double) -> Double -> Double -> Double -> Double
funcWrapper func x' y' z' = func (toR x' y' z', toTau x' y' z', toPhi x' y' z')

