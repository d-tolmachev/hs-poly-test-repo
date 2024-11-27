module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x
    | abs x <= 2 * pi = tc_sin 0.0 16 x
    | otherwise = tc_sin 0.0 16 $ trunc x
    where
        tc_sin acc 0 x = acc + x
        tc_sin acc i x = tc_sin (acc + (((fromInteger $ tc_pow 1 (-1) i) / (fromInteger $ tc_fact 1 $ 2 * i + 1)) * (tc_pow 1.0 x $ 2 * i + 1))) (i - 1) x
        tc_pow acc _ 0 = acc
        tc_pow acc base exp = tc_pow (acc * base) base (exp - 1)
        tc_fact acc 0 = acc
        tc_fact acc n = tc_fact (acc * n) (n - 1)
        trunc x = x - 2 * pi * (fromInteger $ floor $ x / (2 * pi))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x
    | abs x <= 2 * pi = tc_cos 0.0 16 x
    | otherwise = tc_cos 0.0 16 $ trunc x
    where
        tc_cos acc 0 _ = acc + 1.0
        tc_cos acc i x = tc_cos (acc + (((fromInteger $ tc_pow 1 (-1) i) / (fromInteger $ tc_fact 1 $ 2 * i)) * (tc_pow 1.0 x $ 2 * i))) (i - 1) x
        tc_pow acc _ 0 = acc
        tc_pow acc base exp = tc_pow (acc * base) base (exp - 1)
        tc_fact acc 0 = acc
        tc_fact acc n = tc_fact (acc * n) (n - 1)
        trunc x = x - 2 * pi * (fromInteger $ floor $ x / (2 * pi))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = myGCD b $ a `mod` b

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | day <= 28 && month == 2 = True
    | day <= 29 && month == 2 = isLeapYear year
    | day <= 30 && month `elem` [4, 6, 9, 11] = True
    | day <= 31 && month `elem` [1, 3, 5, 7, 8, 10, 12] = True
    | otherwise = False
    where
        isLeapYear year = (year `mod` 400 == 0) || (year `mod` 4 == 0 && year `mod` 100 /= 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base exp = tc_pow 1 base exp
    where
        tc_pow acc _ 0 = acc
        tc_pow acc base exp = tc_pow (acc * base) base (exp - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x = checkPrime x $ ceiling $ sqrt $ fromInteger x
    where
        checkPrime _ 1 = True
        checkPrime x i = if x `mod` i /= 0 then checkPrime x $ i - 1 else False

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = (/2) $ abs $ tc_shapeAria 0.0 points $ head points
    where
        tc_shapeAria acc [(xn, yn)] (x0, y0) = acc + xn * y0 - x0 * yn
        tc_shapeAria acc ((xi, yi) : (xi1, yi1) : points) p0 = tc_shapeAria (acc + xi * yi1 - xi1 * yi) ((xi1, yi1) : points) p0

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a <= b && b <= c = getTriangleKind a b c
    | a <= c && c <= b = getTriangleKind a c b
    | b <= a && a <= c = getTriangleKind b a c
    | b <= c && c <= a = getTriangleKind b c a
    | c <= a && a <= b = getTriangleKind c a b
    | c <= b && b <= a = getTriangleKind c b a
    where
        getTriangleKind a b c
            | a + b < c = -1
            | a * a + b * b < c * c = 0
            | a * a + b * b > c * c = 1
            | a * a + b * b == c * c = 2
