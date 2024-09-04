
-- Ejercicio 1
--a
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

--b
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- Ejercicio 2
absoluto :: Integer -> Integer
absoluto n  | n >= 0 = n 
            | n < 0 = modulo 
            where modulo = (-1)*n

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m  | n > m = n
                    | m > n = m
    
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z   | x>=y && x>=z = x
                | y>=x && y>=z = y
                | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b   | a < 1 || b < 1 = True
                | otherwise = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b   | a < 1 && b < 1 = True
                | otherwise = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo a b  | a > 7 && b > 7 = True
                    | (a>3 && a<7) && (b>3 && b<7) = True
                    | a <= 3 && b <= 3 = True
                    | otherwise = False

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x/=y && x/=z && y/=z = (x+y+z)
                    | 