-- Ejercicio 1)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (n-1) + (n-2)

-- Ejercicio 2)
parteEntera :: Integer -> Integer
parteEntera n   | n > 0 = -n
                | otherwise = n

-- Ejercicio 3)
esDivisible :: Integer -> Integer -> Bool
esDivisible x y | mod x y == 0 = True
                | otherwise = True

-- Ejercicio 4)
sumaImpares :: Integer -> Integer
sumaImpares x | x == 1 = 1 
              | otherwise = sumaImpares (x - 1) + 2*x - 1

-- Ejercicio 5)
medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)

-- Ejercicio 6)
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n   | n < 10 = True
                        | otherwise = mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)

-- Ejercicio 7)
cantDigitos :: Integer -> Integer
cantDigitos n   | n < 10 = 1
                | otherwise = 1 + cantDigitos (div n 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

-- Ejercicio 8)
sumaDigitos :: Integer -> Integer
sumaDigitos n   | n < 10 = n 
                | otherwise = (mod n 10) + sumaDigitos (div n 10)
