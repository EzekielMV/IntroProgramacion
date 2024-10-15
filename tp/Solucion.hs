module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

------EJERCICIO 1
vueloValido :: Vuelo -> Bool
vueloValido (ciudad,ciudad1,duracion) = (ciudad /= ciudad1 && duracion > 0)

pertenece :: AgenciaDeViajes -> Vuelo -> Vuelo -> Bool   -- Este pertenece no cuenta los tiempos de duracion del vuelo solo tiene en cuenta el partida y llegada
pertenece [] _ _ = False
pertenece ((partida,llegada,_):ms) (origen,destino,_) vuelo | partida == origen && llegada == destino = True
                                                            | otherwise = pertenece ms vuelo vuelo

cumpleVueloValido :: AgenciaDeViajes -> Bool
cumpleVueloValido [] = True
cumpleVueloValido (vuelo1:vuelos) | vueloValido vuelo1 = cumpleVueloValido vuelos
                                  | otherwise = False

vuelosValidosAux :: AgenciaDeViajes -> Bool
vuelosValidosAux [n] = True
vuelosValidosAux (vuelo1:vuelo2:vuelos)    
    | vuelo1 == vuelo2 = False
    | not (pertenece (vuelo2:vuelos) vuelo1 vuelo1) = vuelosValidosAux (vuelo2:vuelos)
    | otherwise = False

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos vuelos = vuelosValidosAux vuelos && cumpleVueloValido vuelos

-- Ejercicio 2
ciudadesConectadasConRepetidos :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadasConRepetidos [] _ = []
ciudadesConectadasConRepetidos ((org,dst,_):vuelos) cd  
        | org == cd = dst: ciudadesConectadasConRepetidos vuelos cd 
        | dst == cd = org: ciudadesConectadasConRepetidos vuelos cd
        | otherwise = ciudadesConectadasConRepetidos vuelos cd

eliminarRepetidos :: [Ciudad] -> [Ciudad]
eliminarRepetidos [] = []
eliminarRepetidos [n] = [n]
eliminarRepetidos (n:m:ns) | n == m = eliminarRepetidos (n:ns)
                            | otherwise = n: eliminarRepetidos (m:ns)

ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = []
ciudadesConectadas agencia cd = eliminarRepetidos(ciudadesConectadasConRepetidos agencia cd)


-- EJERCICIO 3
diezPorCiento :: Float -> Float
diezPorCiento n = (10*n)/100

menosTiempo :: Vuelo -> Vuelo
menosTiempo (origen,destino,duracion) = (origen,destino,duracion - (diezPorCiento duracion))

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota (v1:vuelos) = (menosTiempo v1) : modernizarFlota vuelos

-- EJERCICIO 4
aplanar :: AgenciaDeViajes -> [String]
aplanar [] = []
aplanar ((origen,destino,_):ag) = [origen] ++ [destino] ++ aplanar ag

cuentaApariciones :: Ciudad -> [String] -> Integer
cuentaApariciones _ [] = 0
cuentaApariciones ciudad (c1:ciudades) 
    | ciudad == c1 = 1 + cuentaApariciones ciudad ciudades
    | otherwise = cuentaApariciones ciudad ciudades

masApariciones :: [String] -> Ciudad
masApariciones [] = " "
masApariciones [x] = x
masApariciones (c1:c2:ciudades)
    | cuentaApariciones c1 (c1:c2:ciudades) >= cuentaApariciones c2 (c1:c2:ciudades) = masApariciones (c1:ciudades)
    | otherwise = masApariciones (c2:ciudades)

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [] = " "
ciudadMasConectada agencia = masApariciones (aplanar agencia)

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = rutaDirecta vuelos origen destino || rutaConEscala vuelos origen destino vuelos


rutaDirecta :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
rutaDirecta [] _ _ = False
rutaDirecta ((partida, llegada, _):vs) origen destino 
    | partida == origen && llegada == destino = True
    | otherwise = rutaDirecta vs origen destino

rutaConEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes -> Bool
rutaConEscala [] _ _ _ = False
rutaConEscala ((partida, llegada, _):vs) origen destino vuelos
    | partida == origen = rutaDirecta vuelos llegada destino || rutaConEscala vs origen destino vuelos
    | otherwise = rutaConEscala vs origen destino vuelos

-- EJERCICIO 6
duracionRutaDirecta :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionRutaDirecta [] _ _ = 0
duracionRutaDirecta ((partida, llegada,duracion):vs) origen destino | partida == origen && llegada == destino = duracion
                                                                    | otherwise = duracionRutaDirecta vs origen destino

duracionRutaConEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Float
duracionRutaConEscala [] _ _ = 0
duracionRutaConEscala ((partida, llegada,duracion):vs) origen destino
    | partida == origen && rutaDirecta vs llegada destino = duracion + (duracionRutaDirecta vs llegada destino)
    | otherwise = duracionRutaConEscala vs origen destino



duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido [] _ _ = 0
duracionDelCaminoMasRapido agencia origen destino 
                                | duracionDirecto /= 0 && duracionEscala /= 0 && duracionDirecto < duracionEscala = duracionDirecto
                                | duracionDirecto /= 0 && duracionEscala /= 0 &&  duracionDirecto > duracionEscala = duracionEscala 
                                | duracionEscala == 0 = duracionDirecto
                                | otherwise = duracionEscala
                                where duracionDirecto = duracionRutaDirecta agencia origen destino 
                                      duracionEscala = duracionRutaConEscala agencia origen destino

-- EJERCICIO 7
