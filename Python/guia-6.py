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
def peso_pino(metros:int)->int:
    altura = metros*100
    if altura <= 3 * 100:
        resultado = altura * 3
    else:
        resultado = 300 * 3 + (altura - 300) * 2 
    return resultado

def es_peso_util(peso):
    peso_max:int = 1000
    peso_min:int = 400
    return peso_min <= peso <= peso_max

def sirve_pino(altura):
    peso_max:int = 100
    peso_min:int = 400
    return peso_min <= peso_pino(altura) <= peso_max

def sirve_pino2(altura):
    return es_peso_util(peso_pino(altura))

# Ejercicio 5 -- Utilizanod las condicionales if/else
def devolver_el_doble_si_es_par(numero):
    if numero % 2 == 0:
        res = numero * 2
    else:
        res = numero
    return res

def devolver_valor_si_es_par_sino_el_que_sigue(numero):
    if numero % 2 == 0:
        res = numero
    else: 
        res = numero + 1
    return res

def devolver_el_doble_si_es_multiplo3_el_triple_si_es_multiplo9(numero): 
    if numero % 3 == 0: 
        res = numero * 2
    elif numero % 9 == 0:
        res = numero * 3
    else: 
        res = numero
    return res

def lindo_nombre(nombre):
    if len(nombre) >= 5:
        print("Tu nombre tiene muchas letras")
    else:
        print("Tu nombre tiene menos de 5 caracteres")

def elRango(numero):
    if numero < 5:
        print("Menor a 5")
    elif 10 <= numero <= 20:
        print("Entre 10 y 20")
    elif numero > 20:
        print("Mayor a 20")

def jubilarse(edad:int, sexo:str):
    vacaciones = "Ve de vacaciones"
    if edad < 18 and sexo == "M" or sexo == "F":
        print(vacaciones)
    elif sexo == "F" and edad > 60:
        print(vacaciones)
    elif sexo == "M" and edad > 65:
        print(vacaciones)
    else:
        print("Ve a trabajar")

# Ejercicio 6
def unoal10():
    i:int = 0
    while i < 10:
        i += 1
        print(i)

def diez_a_cuarenta():
    i:int = 10
    while i < 40:
        i += 2
        print(i)

def eco_diezveces():
    i:int = 0
    while i < 10:
        print("Eco")
        i += 1

def cuenta_regresiba():
    i:int = 11
    while i > 1:
        i -= 1
        print(i)
    print("Despegue")

def viaje_en_el_tiempo(año_de_llegada:int, año_de_partida:int):
    while año_de_partida < año_de_llegada :
        año_de_llegada -= 1
        print(f'Viajo un año al pasado, estamos en el año {año_de_llegada}')
    print(f"Llegamos al año que querias. ¿Estas feliz?")

viaje_en_el_tiempo(2024,2017)
