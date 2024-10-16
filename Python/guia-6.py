import math
# Ejercicio 1
def imprimir_hola_mundo():
    mensaje = "Hola mundo"
    print(mensaje)

def imprimir_un_verso():
    mensaje = "Hola, ¿Cómo estas?.\nSe que estas bien. \nMe alegro :)" 
    return mensaje

def raizDe2(num:int):
    res = round(math.sqrt(num), 4)
    return res

def factorial_de_dos():
    res = math.factorial(2)
    return res

def perimetro(): 
    res = round(2*math.pi,5)
    return res

# Ejercicio 2)
def imprimir_saludo(nombre:str)->str:
    res = f"Hola {nombre}"
    return res

def raiz_cuadrada_de(numero:int)->float:
    res = math.sqrt(numero)
    return res

def fahrenheit_a_celsius(temp_far:float)->float:
    res = round((temp_far - 32) * 5/9, 2)
    return res

def imprimir_dos_veces(estribillo:str)->str:
    res = 2 * estribillo
    return res

def es_multiplo_de(n:int, m:int)->bool:
    resto_n_m = n % m
    return resto_n_m == 0

def es_par(n:int) -> bool:
    return es_multiplo_de(n,2)

def cantidad_de_pizzas(comensales:int, min_cant_de_porciones:int):
    return round((comensales*min_cant_de_porciones)/8)

# Ejercicio 3
def alguno_es_0(numero1:float,numero2:float) -> bool:
    return numero1 or numero2 == 0

def ambos_son_0(numero1:float, numero2:float) -> bool:
    return numero1 and numero2 == 0

def es_nombre_largo(nombre:str) -> bool:
    return 3 <= len(nombre) <= 8

def es_bisiesto(year):
    return es_multiplo_de(year,400) or es_multiplo_de(year,4) and not es_multiplo_de(year,100)

# Ejercicio 4
