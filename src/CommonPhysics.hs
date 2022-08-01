module CommonPhysics where

import           Data.List
import           Mathematics


{--################__CONSTANTS__#######################--}

--Постоянная Планка
hPlank :: Double
hPlank = 6.62607015*10**(-34)
--Радиус Бора
aBohr :: Double
--aBohr = 52.917720859*(10**(-12))
aBohr = 1.0
--Постоянная тонкой структуры
aThin :: Double
aThin  = 7.2973525693*10**(-3)
--Заряд электрона
eCharge :: Double
eCharge = 1.60217663 * 10**(-19)


{--################__BASIC FUNCTIONS__#######################--}

--Проверка на нормальность в сферических координатах
normSpher :: ((Double, Double, Double) -> Double)
    -> Coord -> Double
normSpher func = integralSpherical (abs . (**2.0) . func)

isNormalSper :: ((Double, Double, Double) -> Double)
    -> Coord -> (Double, Bool)
isNormalSper func coord
    |  abs (normSpher func coord) >= 1-0.03 && abs (normSpher func coord) <= 1+0.03
                                                    = (normSpher func coord, True)
    |  otherwise = (normSpher func coord, False)

{--################__OPERATORS PROPERTIES__#######################--}

--Проверка на коммутативность
isCommutator :: (Eq a, Num a) => ((t -> a) -> t -> a)-> ((t -> a) -> t -> a) -> (t -> a) -> t -> Bool
isCommutator operator1 operator2 func x
    | (operator1 . operator2) func x - (operator2 . operator1) func x == 0 = True
    | otherwise = False

--Собственные значения
ownValues :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> [Double] -> [Double]
ownValues operator func = map (\x ->operator func x / func x)

--Среднее значение некоторой величины
averageValue :: (Double -> Double) -> ((Double -> Double) -> Double -> Double) -> [Double] -> Double
averageValue func operator = integral (\x -> func x * operator func x)

{--################__NON-MATHEMATICS HELPERS__#######################--}
-- Заменяем точки на запятые, а то я затрахался их вручную менять
repldot :: [String] -> [String]
repldot = map helper
    where helper "." = ","
          helper c   = c

-- I think there is no need to manualy change angles steps and borders
-- readSpec :: String -> Coord
-- readSpec string = Coord [0.0, sR .. uR] [0.0, sTau .. pi] [0.0, sPhi .. 2.0*pi]
--                     where sR,uR,sTau,sPhi :: Double
--                           sR   = readElem 1 (read string :: (Double,Double,Double,Double))
--                           uR   = readElem 2 (read string :: (Double,Double,Double,Double))
--                           sTau = readElem 3 (read string :: (Double,Double,Double,Double))
--                           sPhi = readElem 4 (read string :: (Double,Double,Double,Double))

readSpec :: String -> Coord
readSpec string = Coord [0.0, sR .. uR] [0.0, 0.01 .. pi] [0.0, 0.01 .. 2.0*pi]
                    where sR,uR :: Double
                          sR   = fst (read string :: (Double,Double))
                          uR   = snd (read string :: (Double,Double))



-- readElem :: (Eq a1, Num a1) => a1 -> (a2, a2, a2, a2) -> a2
-- readElem n (f,s,t,fr)
--     | n == 1 = f
--     | n == 2 = s
--     | n == 3 = t
--     | n == 4 = fr
-- readElem _ _ = error "Something wrong"

