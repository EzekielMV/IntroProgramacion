#4
def maximo(s:list[int])->int:
    return max(s)

def maximo_for(s:list[int]):
	maximo:int = s[0]
	for i in s:
		if i > maximo:
			maximo = i
	return maximo

def maximo_while(s:list[int]):
    i:int = 0
    maximo:int = s[0]
    while i < len(s):
        if s[i] > maximo:
            maximo = s[i]
        i+=1
    return maximo

def maximo_for_in_range(s:list[int]):
    maximo:int = s[0]
    for i in range(len(s)):
        if s[i] > maximo:
            maximo = s[i]
    return maximo
    
#5 La idea es la misma idea pero teniendo en cuenta los minimos.
def minimo(s:list[int])->int:
    return min(s)

def minimo_for(s:list[int]):
	maximo:int = s[0]
	for i in s:
		if i < maximo:
			maximo = i
	return maximo

def minimo_while(s:list[int]):
    i:int = 0
    maximo:int = s[0]
    while i < len(s):
        if s[i] < maximo:
            maximo = s[i]
        i+=1
    return maximo

def minimo_for_in_range(s:list[int]):
    maximo:int = s[0]
    for i in range(len(s)):
        if s[i] < maximo:
            maximo = s[i]
    return maximo

#6
def ordenados(s:list[int])->bool:
    return s == sorted(s)

def ordenados_for(s:list[int])->bool:
    for i in s:
        if s[0] > i:
            return False
    return True

def ordenados_while(s:list[int])->bool:
    i:int = 0
    while i < (len(s)-1):
        if s[i] > s[i+1]:
            return False
        i += 1
    return True

def ordenados_for_in_range(s:list[int])->bool:
    for i in range(len(s)-1):
        if s[i] > s[i+1]:
            return False
    return True

#7
# Tambien veamos si hay una funcion.
# def por_maximo_for(s:list[int])-int: ----> mas dudas que soluciones.

def pos_maximo_while(s:list[int])->int:
    i:int=0
    while i < len(s):
        if s[i] == maximo_while(s):
              return i
        i += 1
    return -1

def pos_maximo_for_in_range(s:list[int])->int:
    for i in range(len(s)):
        if s[i] == maximo_for_in_range(s):
             return i
    return -1 

#8
# Tambien veamos si hay una funcion.
# def pos_minimo_for(s:list[int])-int: ----> mas dudas que soluciones.

def pos_minimo_while(s:list[int])->int:
    i:int=0
    while i < len(s):
        if s[i] == minimo_while(s):
              return i
        i += 1
    return -1

def pos_minimo_for_in_range(s:list[int])->int:
    for i in range(len(s)):
        if s[i] == minimo_for_in_range(s):
             return i
    return -1 

#9
def long_char_mayor_a_7_for(s:list[str])->bool:
    for i in s:
        if len(i) > 7:
            return True
    return False

def long_char_mayor_a_7_while(s:list[str])->bool:
    i:int = 0
    while i < len(s):
        if len(i) > 7:
            return True
    return False

def long_char_mayor_a_7_for_in_range(s:list[int])->bool:
    for i in range(len(s)):
        if len(i) > 7:
             return False
    return False
