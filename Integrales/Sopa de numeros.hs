type Fila = [Int]
type Columna = Int
type Tablero = [Fila]
type Posicion = (Int, Columna)
type Camino = [Posicion]

maximoEnLista :: [Int] -> Int
maximoEnLista [n] = n
maximoEnLista (n:y:ms)  | n >= y = maximoEnLista (n:ms)
                        | otherwise = maximoEnLista (y:ms)

repetidos :: Int -> [Int] -> Int
repetidos _ [] = 0
repetidos m (n:ms)| m == n = 1 + repetidos m ms
                 | otherwise = repetidos m ms 

masRepetidosEnLista :: [Int] -> Int
masRepetidosEnLista [n] = n
masRepetidosEnLista (n:y:ms) | repetidos n (n:y:ms) >= repetidos y (n:y:ms) = masRepetidosEnLista (n:ms)
                             | otherwise = masRepetidosEnLista (y:ms)

-- Ejercicio 1)
maximo :: Tablero -> Int
maximo [n] = maximoEnLista n
maximo tablero = maximoEnLista (listaDeMaximos tablero)
            where   listaDeMaximos :: [[Int]] -> [Int]
                    listaDeMaximos [] = []
                    listaDeMaximos (n:ms) = maximoEnLista n: listaDeMaximos ms

-- Ejercicio 2)
listaDeRepetidos :: Tablero -> Fila
listaDeRepetidos [] = []
listaDeRepetidos (m:ms) = m ++ listaDeRepetidos ms

masRepetidos :: Tablero -> Int
masRepetidos [n] = masRepetidosEnLista n
masRepetidos n = masRepetidosEnLista (listaDeRepetidos n)

-- Ejercicio 3)
-- Función para obtener una fila del Tablero dada una posición
obtenerFila :: Tablero -> Int -> Fila 
obtenerFila [fila] _ = fila   --Si yo  tengo solo esa fila devuelvo  esa
obtenerFila (fila:tablero) 1 = fila  --Como  empiezo  a  contar  de  1  devuelvo  la primera fila,  por  mas  que  Haksell  cuente  desde  0
obtenerFila (fila:tablero) n = obtenerFila tablero (n - 1) --Paso recursivo donde voy retrocediendo hasta llegar al caso base 

-- Función para obtener un valor dentro de una Fila dada la columna
obtenerValorEnColumna :: Fila -> Int -> Int
obtenerValorEnColumna (valor1:fila) 1 = valor1 --Esto es  porque  es  mi  camino  las posiciones  va a   empezar  en  1  no es  0  como  cuenta  Haskell
obtenerValorEnColumna (valor1:fila) n = obtenerValorEnColumna fila (n - 1) --Este es  el  paso  recursivo  para moverme  dentro  de la fila

-- Función principal que toma un Tablero y un Camino y devuelve los valores del Camino
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = [] --Si  mi  camino  es  vacio  devuelvo  la  lista   vacia   
valoresDeCamino tablero ((f, c):camino) = obtenerValorEnColumna (obtenerFila tablero f) c : valoresDeCamino tablero camino --Obtengo el  valor  de la  coordenada y  lo  concateno

-- Ejercicio 4)
longitud :: [Int] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

fibonacci :: Int -> Int
fibonacci(0) = 0
fibonacci(1) = 1
fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)

listaFibonacci :: Int -> Int -> [Int]
listaFibonacci _ 0 = []
listaFibonacci n m = fibonacci n : listaFibonacci (n+1) (m-1)

esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [] _ = False 
esCaminoFibo s i | listaFibonacci i (longitud s) == s = True
                 | otherwise = False