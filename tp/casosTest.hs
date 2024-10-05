import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "vuelo válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
    "vuelo no válido con un elemento" ~: vuelosValidos [("BsAs", "BsAs", 5.0)] ~?= False,
    "vuelos válidos con ciudades distintas" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("Bariloche","Córdoba",3.4)] ~?= True,
    "vuelos válidos con ida y vuelta" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("Rosario","BsAs",5.0)] ~?= True,
    "vuelos válidos con una ciudad repetida" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("BsAs","Córdoba",3.4)] ~?= True,
    "vuelos no válidos con igual origen y destino, distinta duracion" ~: vuelosValidos [("BsAs", "Rosario", 5.0),("BsAs","Rosario",4.5)] ~?= False
    ]

testsEjciudadesConectadas = test [
    "ciudades conectadas por origen" ~: ciudadesConectadas  [("BsAs", "Rosario", 10.0)] "Rosario" ~?= ["BsAs"],
    "ciudades conectadas por destino" ~: ciudadesConectadas  [("Rosario", "BsAs", 8.0)] "Rosario" ~?= ["BsAs"],
    "ciudad conectada con dos destinos (y extra)" ~: expectPermutacion(ciudadesConectadas  [("BsAs", "Rosario", 5.0),("BsAs","Bariloche",3.4),("Rosario","Cordoba",4.3)] "BsAs") ["Rosario","Bariloche"],
    "ciudad conectada con dos origenes (y extra)" ~: expectPermutacion(ciudadesConectadas  [("Rosario", "BsAs", 5.0),("Bariloche","BsAs",3.4),("Rosario","Cordoba",4.3)] "BsAs") ["Rosario","Bariloche"],
    "ciudad conectada con origen y destino (y extra)" ~: expectPermutacion(ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Bariloche","BsAs",3.4),("Rosario","Cordoba",4.3)] "BsAs") ["Rosario","Bariloche"],
    "ciudad conectada con ciudad repetida" ~: expectPermutacion(ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Bariloche","BsAs",3.4),("Rosario","BsAs",5.0)] "BsAs") ["Rosario","Bariloche"],
    "ciudad conectada con varias ciudades" ~: expectPermutacion(ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Bariloche","BsAs",3.4),("Córdoba","BsAs",5.3)] "BsAs") ["Rosario","Bariloche","Córdoba"],
    "ciudad que no está en la agencia" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Córdoba" ~?= []
    ]

testsEjmodernizarFlota = test [
    "flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)]
    ]

testsEjciudadMasConectada = test [
    "ciudad Mas conectada caso base" ~: ciudadMasConectada [] ~?= " ",
    "ciudad Mas conectada que aparece dos veces como origen" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("BsAs", "Córdoba", 7.0)] ~?= "BsAs",
    "ciudad Mas conectada que aparece dos veces como destino" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Córdoba", "Rosario", 7.0)] ~?= "Rosario",
    "ciudad Mas conectada que aparece como orig y destino" ~: ciudadMasConectada [("Córdoba", "Rosario", 10.0), ("BsAs", "Córdoba", 7.0)] ~?= "Córdoba",
    "ciudad Mas conectada que aparece varias veces como orig y dest" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0),("Bariloche","Rosario",10.9)] ~?= "Rosario",
    "ciudad Mas conectada con un elemento" ~: expectAny(ciudadMasConectada [("BsAs", "Rosario", 10.0)]) ["Rosario","BsAs"],
    "ciudad Mas conectada con misma cantidad de apariciones" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "BsAs", 7.0)]) ["Rosario","BsAs"],
    "ciudad Mas conectada con misma cantidad de apariciones +extra" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "BsAs", 7.0),("Bariloche","Córdoba",20.1)]) ["Rosario","BsAs"]
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso base" ~: sePuedeLlegar [] "BsAs" "Tucumán" ~?= False,
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "Rosario" "BsAs" ~?= True,
    "Se puede llegar caso verdadero con vuelo directo" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Rosario" ~?= True,
    "Se puede llegar caso falso, origen no está en agencia" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "Bariloche", 8.0)] "Tucumán" "BsAs" ~?= False,
    "Se puede llegar caso falso, destino no está en agencia" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "Bariloche", 8.0)] "BsAs" "Tucumán" ~?= False,
    "Se puede llegar caso falso, con más de una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "Tucumán", 8.0)] "BsAs" "Tucumán" ~?= False,
    "Se puede llegar origen al final" ~: sePuedeLlegar [("Córdoba", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("BsAs", "Rosario", 5.0)] "BsAs" "Córdoba" ~?= True,
    "Se puede llegar origen en el medio" ~: sePuedeLlegar [("Córdoba", "Rosario", 5.0), ("BsAs", "Rosario", 5.0),("Rosario", "Córdoba", 5.0)] "BsAs" "Córdoba" ~?= True
   ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 10.0,
    "duración del camino más rápido con vuelo directo sin escalas" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("BsAs", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 5.0
    ]

testsEjpuedoVolverAOrigen = test [
        "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True
    ]



-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
