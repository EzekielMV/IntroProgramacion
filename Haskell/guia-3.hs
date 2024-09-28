import GHC.Core.Map.Expr (TrieMap)

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
            | n < 0 = (-1)*n

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto n m  | n > m = n
                    | m > n = m
    
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z   | x >= y && x >= z = x
                | y >= x && y >= z = y
                | otherwise = z
--d)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b   | a == 1 || b == 0 = True
                | otherwise = False
-- Con pattern matching
algunoEs0_1 :: Float -> Float -> Bool
algunoEs0_1 n 0 = True
algunoEs0_1 0 n = True
algunoEs0_1 n m = False

--e)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b   | a == 1 && b == 1 = True
                | otherwise = False
-- Con pattern matching
ambosSon0_1 :: Float -> Float -> Bool
ambosSon0_1 0 0 = True
ambosSon0_1 n 0 = False
ambosSon0_1 0 m = False
ambosSon0_1 x y = False

--f)
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo a b  | a > 7 && b > 7 = True
                    | (a > 3 && a < 7) && (b > 3 && b < 7) = True
                    | a <= 3 && b <= 3 = True
                    | otherwise = False

--g)
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y /= z = x + y + z
                    | x == y && x /= z = x + z
                    | x == z && x /= y = x + y
                    | y /= z && y == x = y + z
                    | otherwise = 0


--h)
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b    | mod a b == 0 = True
                    | otherwise = False

--i)
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

--j)
digitoDecenas :: Int -> Int
digitoDecenas x = digitoDecenas (div x 10)

-- Ejercicio 3)
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b   | mod a b == 0 = True
                        | otherwise = False

-- Ejercicio 4)
type Punt2D = (Float,Float)
--a)
proInd :: (Int, Int) -> (Int, Int) -> Int
proInd (a,b) (c,d) = (a*c) + (b*d)

--b)
todoMenor :: (Int,Int)->(Int,Int)->Bool
todoMenor (x,y) (x1,y1) | x > x1 && y > y1 = True
                        | otherwise = False 
    
--c)
distanciaPuntos :: Punt2D -> Punt2D -> Float
distanciaPuntos (x,y) (a,b) = sqrt((a-x)^2+(b-y)^2)

--d)
sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (x,y,z) = x + y + z

--e) 
sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (a,b,c) n    
        | mod a n == 0 && mod b n == 0 && mod c n == 0 = a + b + c     
        | mod a n == 0 && mod b n == 0 && mod c n /= 0 = a + b
        | mod a n /= 0 && mod b n == 0 && mod c n == 0 = b + c
        | mod a n == 0 && mod b n /= 0 && mod c n == 0 = a + c
        | mod a n == 0 = a
        | mod b n == 0 = b
        | mod c n == 0 = c
        | otherwise = 0

--f)
posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (a,b,c)    | mod a 2 == 0 = 0
                        | mod b 2 == 0 = 1
                        | mod c 2 == 0 = 2
                        | otherwise = 4

--g)
crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

--h) 
invertir :: (a,b) -> (b,a)
invertir (a,b)=(b,a)

--Ejercicio 5)
todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (a,b,c)    | (f2 a) > (g2 a) && (f2 b) > (g2 b) && (f2 c) > (g2 c) = True
                        | otherwise = False

f2 :: Integer -> Integer
f2 n | n <= 7 = n^2
    | n > 7 = 2*n-1

g2 :: Integer -> Integer
g2 n | mod n 2 == 0 = div n 2
    | otherwise = 3*n+1

--Ejercicio 6)
type Anio = Integer
type EsBinisiesto = Bool

bisiesto :: Anio -> EsBinisiesto
bisiesto año    | mod año 100 == 0 && mod año 400 == 0 = True
                | mod año 4 == 0 && mod año 100 /= 0 = True
                | otherwise = False

--Ejercicio 7)
--a)
type Coordenada3d = (Float,Float,Float)

distanciaManhattan :: Coordenada3d -> Coordenada3d -> Float
distanciaManhattan (x1,x2,x3) (y1,y2,y3) = (abs(x1 - y1)) + (abs(x2 - y2)) + (abs (x3 - y3))

--Ejercicio 8)
comparar :: Integer -> Integer -> Integer
comparar a b    | sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b) = 1
                | sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b) = (-1)
                | sumaUltimosDosDigitos(a) == sumaUltimosDosDigitos(b) = 0

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = (mod (absoluto x) 10) + (mod (div (absoluto x) 10) 10)