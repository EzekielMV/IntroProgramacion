module Guia5 where
import GHC.Core (otherCons)

eliminar :: Integer -> [Integer] -> [Integer]
eliminar _ [] = []
eliminar m (n:ns)   | m == n = eliminar m ns
                    | otherwise = eliminar m ns

--Ejercicio 1)
-- A)
longitud :: [t] -> Integer
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

-- B)
ultimo :: [t] -> t
ultimo [n] = n
ultimo (n:ns) = ultimo ns

-- C)
principio :: [t] -> [t]
principio [n] = [n] 
principio (n:ns) = ns

-- D)
reverso :: [t] -> [t]
reverso [n] = [n]
reverso (n:ns) = reverso ns ++ [n]

-- Ejercicio 2)
--1)
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (m:ms)  | n == m = True
                    | otherwise = pertenece n (ms)

--2)
todosIguales :: (Eq t) => [t] -> [t] -> Bool
todosIguales [] [] = False
todosIguales (n:ns) (m:ms)  | n == m = True
                            | n /= m = False
                            | otherwise = todosIguales ns ms

--3)
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [n] = False
todosDistintos (n:y:ns) | n == y = False
                        | otherwise = todosDistintos (y:ns)

--4)
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [n] = False
hayRepetidos (n:y:ns)   | n == y = True
                        | otherwise = hayRepetidos (y:ns)

--5)
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (m:ms) | n == m = ms
                | otherwise = m:quitar n ms

--6)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (s:sn) | e == s = quitarTodos e sn
                     | otherwise = s:quitarTodos e (sn)

--7)
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (n:y:ns)  | n == y = (n:ns)
                            | otherwise = y:eliminarRepetidos (n:ns)
    
--8)
--mismosElementos :: (Eq t) => [t] -> [t] -> Bool
--mismosElementos [] [] = True
--mismosElementos (n:ns) (m:ms)   | pertence n (m:ms) && pertenece m (n:ns) && todosIguales (n:ns) (m:ms) = True

--9)
capicua :: (Eq t) => [t] -> Bool
capicua [] = False
capicua s   | s == reverso s = True
            | otherwise = False

-- Ejercicio 3)
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

productoria :: [Integer] -> Integer 
productoria [] = 0
productoria (n:ns) = n * productoria ns

maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (n:y:ns)   | n >= y = maximo (n:ns)
                  | otherwise = maximo (y:ns)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (s:ns) = (n+s):(sumarN n ns)

sumarElprimero :: [Integer] -> [Integer]
sumarElprimero [] = []
sumarElprimero (n:ns) = sumarN n ns

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo s = sumarN (ultimo s) s

pares :: [Integer] -> [Integer]
pares [] = []
pares (n:ns) | mod n 2 == 0 = n:pares ns
             | otherwise = pares ns

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (s:ns)   | mod n s == 0 = s:multiplosDeN n ns
                        | otherwise = multiplosDeN n ns

--ordenar :: [Integer] -> [Integer]
--ordenar [x] = [x]
--ordenar (n:ns) = maximo (n:ns) : eliminar n ns

-- Ejercicio 5)
--1)
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [x] = [x]
sumaAcumulada (n:m:ms) = [n] ++ sumaAcumulada ((n+m):ms)

--2)
menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux 1 _ = 1
menorDivisorAux n i     | mod n i == 0 = i
                        | otherwise = menorDivisorAux n (i+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux n 2

esPrimo :: Integer -> Bool
esPrimo n | menorDivisor n == n = True
          | otherwise = False

generarPrimos :: Integer -> [Integer]
generarPrimos 1 = []
generarPrimos n | (esPrimo n == True) = n : generarPrimos (n-1)
                | otherwise = generarPrimos (n-1)


descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (n:ns) = generarPrimos n : descomponerEnPrimos ns 

-- Ejercicio 6)
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos name ((name1,number):ms) | name == name1 = True
                                        | otherwise = enLosContactos name ms

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto _ [] = []
agregarContacto (name,numer) ((name1,numer1):ms) | noEstaEnlosContactos = (name,numer):((name1,numer1):ms)
                                                 | name == name1 = (name,numer) : (ms)
                                                 | otherwise = (name1,numer1):agregarContacto (name,numer) ms
                where noEstaEnlosContactos = not (enLosContactos (name) ((name1,numer1):ms))

eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto _ [] = []
eliminarContacto contacto ((name,number):ms)    | contacto == name = ms
                                                | otherwise = (name,number): eliminarContacto contacto ms

-- Ejercicio 7)
type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker numero ((id,(dis,ubi)):ms) | numero == id = True
                                          | otherwise = existeElLocker numero ms

