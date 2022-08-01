module HydrogenFunctions where

import           CommonPhysics
import           Mathematics

validRange :: Coord
validRange =  spherRange 0.001 6.0 0.01 pi 0.01 (2.0*pi)

functions :: IO ()
functions = do
    putStrLn "Please, enter function's name (e.g. 1s)"
    inputFunc <- getLine
    let func = handlerFunctions inputFunc
    putStrLn "Enter range. Standart - for standart range [0..6.0],  or in format like (step R,upperBorder R)"
    inputRange <- getLine
    let rangeFunc = case inputRange of
                    "Standart" -> validRange
                    _          -> readSpec inputRange
    print $ isNormalSper func rangeFunc

handlerFunctions :: String -> ((Double, Double, Double) -> Double)
handlerFunctions inp
    | inp == "1s" = waveHfunc1s 1.0
    | inp == "2s" =  waveHfunc2s 1.0
    | inp == "2pz"= waveHfunc2pz 1.0
    | inp == "2px" = waveHfunc2px 1.0
    | inp == "2py" = waveHfunc2py 1.0
    | otherwise = error "Unrecognized orbital"

-- #### Hydrogen functions ##########

--1s Hydrogen function
waveHfunc1s :: Double -> (Double, Double, Double) ->  Double
waveHfunc1s z (r, tau, phi) = a*b*e
                            where a,b,e :: Double
                                  a = 1.0/sqrt pi
                                  b = (z/aBohr)**1.5
                                  e = exp(-1.0 * (z*r/aBohr))
--2s Hydrogen function
waveHfunc2s :: Double -> (Double, Double, Double) ->  Double
waveHfunc2s z (r, tau, phi) = a * b * c* e
                            where a,b,c,e :: Double
                                  a = 1.0/(4.0*sqrt (2.0*pi))
                                  b = (z/aBohr)**1.5
                                  c = 2.0-z*r/aBohr
                                  e = exp(-1.0 * (z*r/(2.0*aBohr)))

--2pz Hydrogen function
waveHfunc2pz :: Double -> (Double, Double, Double) ->  Double
waveHfunc2pz z (r, tau, phi) = a * b * c* e
                            where a,b,c,e :: Double
                                  a = 1.0/(4.0*sqrt (2.0*pi))
                                  b = (z/aBohr)**2.5
                                  c = pureTrig cos tau
                                  e = r*exp(-1.0 * (z*r/(2.0*aBohr)))

--2px Hydrogen function
waveHfunc2px :: Double -> (Double, Double, Double) ->  Double
waveHfunc2px z (r, tau, phi) = a * b * c* d * e
                            where a,b,c,e :: Double
                                  a = 1.0/(4.0*sqrt (2.0*pi))
                                  b = (z/aBohr)**2.5
                                  c = pureTrig sin tau
                                  d = pureTrig cos phi
                                  e = r*exp(-1.0 * (z*r/(2.0*aBohr)))

--2py Hydrogen function
waveHfunc2py :: Double -> (Double, Double, Double) ->  Double
waveHfunc2py z (r, tau, phi) = a * b * c* d * e
                            where a,b,c,e :: Double
                                  a = 1.0/(4.0*sqrt (2.0*pi))
                                  b = (z/aBohr)**2.5
                                  c = pureTrig sin tau
                                  d = pureTrig sin phi
                                  e = r*exp(-1.0 * (z*r/(2.0*aBohr)))