import math
# Ejercicio 1
# 1
def imprimir_hola_mundo():
    mensaje = "Hola mundo"
    print(mensaje)

# 2
def imprimir_un_verso():
    mensaje = "Hola, ¿Cómo estas?.\nSe que estas bien. \nMe alegro :)" 
    return mensaje

# 3
def raizDe2(num:int):
    res = round(math.sqrt(num), 4)
    return res

# 4
def factorial_de_dos():
    res = math.factorial(2)
    return res

# 5
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
    k:int = 0
    n == m * k = True
    print()
    
    
    
    
    
    
    
print(imprimir_dos_veces("abc\ndef\nghi\n"))