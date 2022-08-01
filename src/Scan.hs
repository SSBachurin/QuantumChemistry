module Scan where

import           CommonPhysics
import           Graphics.Gnuplot.Simple
import           HydrogenFunctions
import           Mathematics


scan :: IO ()
scan = do
    putStrLn "Please, enter function's name (e.g. 1s)"
    inputFunc <- getLine
    let func = handlerFunctions inputFunc
    putStrLn "Enter Scan start radius value"
    inputRange <- getLine
    print $ scanR func (read inputRange)

scanR :: ((Double, Double, Double) -> Double) -> Double -> Double
scanR func rRange
    | not $ snd (isNormalSper func (rangeForScan rRange)) = scanR func (rRange + 1.0)
    | snd (isNormalSper func (rangeForScan rRange))  = rRange
    | otherwise = error "Something wrong due to the scan"

rangeForScan :: Double -> Coord
rangeForScan rRange = spherRange 0.001 rRange 0.01 pi 0.01 (2.0*pi)