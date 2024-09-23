import Language.Haskell.TH (safe)
import GHC.Builtin.Names (noMethodBindingErrorIdKey)
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
--mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) && pertenece y (x:xs) && todosIguales xs == todosIguales ys = True
--                              | pertenece x (y:ys) && pertenece y (x:xs) = mismosElementos xs ys
--                              | otherwise = False

type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

-- Ejercicio 6)
enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos name ((name1,number):ms) | name == name1 = True
                                        | otherwise = enLosContactos name ms

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto _ [] = []
agregarContacto (name,numer) ((name1,numer1):ms) | noEstaEnlosContactos = (name,numer):((name1,numer1):ms)
                                                 | otherwise = (name,numer1) : agregarContacto (name,numer) ms
                where noEstaEnlosContactos = enLosContactos (name) ((name1,numer1):ms) == False