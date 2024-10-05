import GHC.IO.FD (setNonBlockingMode)
reverso :: [Int] -> [Int]
reverso [] = []
reverso (n:ns) = reverso ns ++ [n]

generarDivisor :: Int -> Int -> [Int]
generarDivisor _ 1 = [1]
generarDivisor n m  | mod n m == 0 = m: generarDivisor n (m-1)
                    | otherwise = generarDivisor n (m-1)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

-- Ejercicio 1)    
divisoresPropios :: Int -> [Int]
divisoresPropios n = reverso (generarDivisor n (n-1))

-- Ejercicio 2)
sonAmigos :: Int -> Int -> Bool
sonAmigos n m | sumatoria (divisoresPropios n) == m && sumatoria (divisoresPropios m) == n = True
              | otherwise = False

-- Ejercicio 3)
esNumeroPerfecto :: Int -> Bool
esNumeroPerfecto 1 = False
esNumeroPerfecto n  | sumatoria (divisoresPropios n ) == n = True
                    | otherwise = False

losPrimerosNPerfectosAux :: Int -> Int -> [Int]
losPrimerosNPerfectosAux _ 0 = []
losPrimerosNPerfectosAux n  m  | esNumeroPerfecto n == True = n : losPrimerosNPerfectosAux (n+1) (m-1)
                            | otherwise = losPrimerosNPerfectosAux (n+1) m

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = losPrimerosNPerfectosAux 1 n

-- Ejercicio 4)
listadoDeAmigos :: [Int] -> [Int] -> [(Int, Int)]
listadoDeAmigos [] [] = []
listadoDeAmigos (n:x:ns) (m:y:ms) | sonAmigos n y == True = (n,y): listadoDeAmigos ns ms
                                  | sonAmigos m x == True = (m,x): listadoDeAmigos ns ms
                                  | otherwise = listadoDeAmigos ns ms