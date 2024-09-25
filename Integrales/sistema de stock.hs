module SistemaDeStock where

-- Funciones auxiliares
apariciones :: String -> [String] -> Int
apariciones _ [] = 0
apariciones n (m:ms)| n == m = 1 + apariciones n ms
                    | otherwise = apariciones n ms

eliminar :: String -> [String] -> [String]
eliminar _ [] = []
eliminar n (m:ms)   | n == m = eliminar n ms
                    | otherwise = m:eliminar n ms


-- Ejercicio 1)
generarStock :: [String] -> [(String,Int)]
generarStock [] = []
generarStock (producto:productos) = (producto, apariciones(producto) (producto:productos)) : generarStock (eliminar producto productos)

-- Ejercicio 2)
stockDeProducto :: [(String, Int)] -> String -> Int
stockDeProducto [] _ = 0
stockDeProducto ((stock_0,stock_1):stock) producto  | producto == stock_0 = stock_1
                                                    | otherwise = stockDeProducto stock producto

-- Ejercicio 3)
dineroEnStock :: [(String,Int)] -> [(String,Float)] -> Float
dineroEnStock 