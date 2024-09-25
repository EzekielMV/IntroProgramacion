module SistemaDeStock where
-- Funciones auxiliares
apariciones :: String -> [String] -> Int
apariciones _ [] = 0
apariciones n (m:ms)| n == m = 1 + apariciones n ms
                    | otherwise = apariciones n ms

generarStock :: [String] -> [(String,Int)]
generarStock [] = []
generarStock (producto:productos) = [(producto,1)]