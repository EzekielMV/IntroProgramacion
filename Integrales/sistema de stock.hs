module SistemaDeStock where
import Data.ByteString.Builder (FloatFormat)
import GHC.Builtin.PrimOps (PrimOp(FloatAcosOp))

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
dineroEnStock [] [] = 0
dineroEnStock (stocks) (productos) = sumatoria (sumarAlista stocks productos)
                where sumatoria :: [Float] -> Float
                      sumatoria [] = 0
                      sumatoria (n:ms) = n + sumatoria ms
                      sumarAlista :: [(String,Int)] -> [(String,Float)]-> [Float]
                      sumarAlista [] [] = []
                      sumarAlista ((a,b):ms) ((c,d):ns) = (fromIntegral b*d):sumarAlista ms ns

-- Ejercicio 4)
ofertaAplicada :: (String, Float) -> (String, Float)
ofertaAplicada (a,b) = (a,(b * 0.80))

aplicarOferta :: [(String, Int)] -> [(String, Float)] -> [(String,Float)]
aplicarOferta [] [] = []
aplicarOferta (stock) ((c,d):ns)| stockDeProducto stock c > 10 = (ofertaAplicada (c,d)): aplicarOferta stock ns
                                | otherwise = (c,d):aplicarOferta stock ns  