module Mathematics where

import           Control.Parallel.Strategies
import           Data.List

data Coord = Coord [Double] [Double] [Double]

{--################__CONSTANTS__#######################--}

--Минимальное отклонение, чтобы занулять тригонометрию
eps :: Double
eps = 10**(-12)
--Отклонение
delta :: Double
delta = 0.001

{--################__SUPPORTING FUNCTIONS__#######################--}

--Правильные тригонометрические функции - с отсечкой значений по модулю меньше эпсилона
pureTrig :: (Double -> Double) -> Double -> Double
pureTrig func x
    | abs(func x)<=eps = 0.0
    | otherwise = func x

--Задание диапазона сферических координат
spherRange :: Double -> Double ->
              Double -> Double ->
              Double -> Double -> Coord
spherRange rSTP rUPB tSTP tUPB pSTP pUPB = Coord [0,rSTP .. rUPB] [0,tSTP .. tUPB] [0,pSTP .. pUPB]

{--################__BASIC FUNCTIONS__#######################--}

--факториал
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

--Производная
derive ::  (Double -> Double) -> Double -> Double
derive f x = (f(x+delta)-f(x-delta))/(2*delta)

--Вторая производная
secondDerive :: (Double -> Double) -> Double -> Double
secondDerive = derive . derive

--Интеграл
integral :: (Double -> Double) -> [Double] -> Double
integral func values = helper * sum (map (\x -> func (x-helper/2.0)) values)
        where
            helper :: Double
            helper = (last values - head values) / fromIntegral (length values)

--Интеграл в сферических координатах
myParMap  ::  (a->b) -> [a] -> Eval [b]
myParMap f [] = return []
myParMap f (a:as) = do
    b <- rpar (f a)
    bs <- myParMap f as
    return (b:bs)

chunkCoord :: Int -> Coord -> [Coord]
chunkCoord n (Coord [] taus phis) = []
chunkCoord n (Coord rs taus phis) = Coord as taus phis : chunkCoord n (Coord bs taus phis)
    where (as,bs) = splitAt n rs

splitCoord :: Int -> Coord -> [Coord]
splitCoord n coord = chunkCoord (length (coordRad coord) `quot` n) coord

coordRad :: Coord -> [Double]
coordRad (Coord r t p) = r

coordTau :: Coord -> [Double]
coordTau (Coord r t p) = t

coordPhi :: Coord -> [Double]
coordPhi (Coord r t p) = p

integralSpherical :: ((Double, Double, Double) -> Double)
                    -> Coord -> Double
integralSpherical func coord
            =   do
                    let s_r = last (coordRad coord) / fromIntegral (length $ coordRad coord)
                        s_phi = last (coordPhi coord) / fromIntegral (length $ coordPhi coord)
                        s_tau = last (coordTau coord) / fromIntegral (length $ coordTau coord)
                        coeff = s_r * s_phi * s_tau
                        calculus (r,  tau, phi) = r**2.0*pureTrig sin tau * func (r+s_r/2.0, tau+s_tau/2.0, phi+s_phi/2.0)
                        mapCalculus (Coord r tau phi) = foldl' (+) 0.0 [calculus (r', tau', phi') | r' <- r, tau' <- tau, phi' <- phi]
                        range = runEval (myParMap mapCalculus (splitCoord 12 coord))
                    coeff * sum range

--Конвертация в декартовы координаты
toX :: Double -> Double -> Double -> Double
toX r tau phi = r* pureTrig sin tau * pureTrig cos phi

toY :: Double -> Double -> Double -> Double
toY r tau phi = r * pureTrig sin tau * pureTrig sin phi

toZ :: Double -> Double -> Double -> Double
toZ r tau phi = r * pureTrig cos tau

--Конвертация в сферические

toR :: Double -> Double -> Double -> Double
toR x y z = sqrt (x^2 + y^2 + z^2)

toTau :: Double -> Double -> Double -> Double
toTau x y z = atan (sqrt  (x^2 + y^2)/z)

toPhi :: Double -> Double -> Double -> Double
toPhi x y z = atan (y/z)