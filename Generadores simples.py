# ===================================================================
# GENERADORES DE NUMEROS PSEUDOALEATORIOS - METODOS CONGRUENCIALES
# ===================================================================
# Todos siguen la misma logica general:
#   Paso 1) __init__   -> aqui guardamos los datos que el generador necesita
#                          (el modulo, los multiplicadores y la semilla)
#   Paso 2) rand(n)     -> aqui generamos n numeros pseudoaleatorios (Ui)
#                          aplicando la formula del generador, paso a paso
#   Paso 3) periodo()   -> aqui calculamos cada cuantos numeros la
#                          secuencia se empieza a repetir
#
# Nota importante: la semilla SI afecta el resultado. Con el mismo
# modulo y multiplicador, una semilla distinta puede dar un periodo
# distinto (por eso periodo() se calcula probando desde la semilla).
# ===================================================================


# ===================================================================
# 1. GENERADOR MULTIPLICATIVO
# Formula:  X(i+1) = (b * Xi) mod m
# ===================================================================
class PseudoRandomMultiplicativeCongruentialGenerator:

    def __init__(self, modulo, multiplicador, semilla):
        # Paso 1: guardamos los datos del generador
        self.modulo = modulo               # esto se usa para el "mod m" de la formula
        self.multiplicador = multiplicador  # esto se usa para multiplicar el estado (b)
        self.estado = semilla               # esto es Xi; empieza en X0 = semilla

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            # aqui aplicamos la formula para obtener el siguiente Xi
            self.estado = (self.multiplicador * self.estado) % self.modulo
            # aqui lo pasamos a un numero entre 0 y 1: Ui = Xi / m
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo desde el estado actual
        estado = self.estado      # variable local, no tocamos self.estado
        visitados = [estado]      # aqui vamos guardando cada Xi que ya salio

        while True:
            estado = (self.multiplicador * estado) % self.modulo

            if estado in visitados:
                # si este Xi ya habia salido antes, ahi termina el ciclo
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


# --- ejemplo de uso ---
ejemplo = PseudoRandomMultiplicativeCongruentialGenerator(15, 7, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# ===================================================================
# 2. GENERADOR MULTIPLICATIVO LINEAL
# Formula:  X(i+1) = (b * Xi + c) mod m
# ===================================================================
class PseudoRandomLinearCongruentialGenerator:

    def __init__(self, modulo, multiplicador, constante, semilla):
        # Paso 1: guardamos los datos del generador
        self.modulo = modulo               # esto se usa para el "mod m"
        self.multiplicador = multiplicador  # esto es b, multiplica al estado
        self.constante = constante          # esto es c, se suma en cada paso
        self.estado = semilla               # esto es Xi; empieza en X0 = semilla

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            # aqui aplicamos la formula lineal para obtener el siguiente Xi
            self.estado = (self.multiplicador * self.estado + self.constante) % self.modulo
            # aqui lo pasamos a un numero entre 0 y 1
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo desde el estado actual
        estado = self.estado
        visitados = [estado]

        while True:
            estado = (self.multiplicador * estado + self.constante) % self.modulo

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


# --- ejemplo de uso ---
ejemplo = PseudoRandomLinearCongruentialGenerator(28, 7, 3, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# ===================================================================
# 3. GENERADOR MULTIPLICATIVO POLINOMIAL
# Formula:  X(i+1) = (b0 + b1*Xi + b2*Xi^2 + ... + bk*Xi^k) mod m
# ===================================================================
class PseudoRandomPolynomialCongruentialGenerator:

    def __init__(self, modulo, coeficientes, semilla):
        # Paso 1: guardamos los datos del generador
        self.modulo = modulo             # esto se usa para el "mod m"
        self.coeficientes = coeficientes  # esta es la lista [b0, b1, ..., bk]
        self.estado = semilla             # esto es Xi; empieza en X0 = semilla

    def polinomio(self, x):
        # esta funcion solo evalua el polinomio b0 + b1*x + b2*x^2 + ...
        resultado = 0

        for i in range(len(self.coeficientes)):
            resultado = resultado + self.coeficientes[i] * x**i

        return resultado

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            # aqui evaluamos el polinomio en el estado actual para obtener el siguiente Xi
            self.estado = self.polinomio(self.estado) % self.modulo
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo desde el estado actual
        estado = self.estado
        visitados = [estado]

        while True:
            estado = self.polinomio(estado) % self.modulo

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


# --- ejemplo de uso ---
coef = [3, 2, 5]
ejemplo = PseudoRandomPolynomialCongruentialGenerator(28, coef, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# ===================================================================
# 4. GENERADOR MULTIPLICATIVO MULTIPLE
# Formula:  Xi = (b1*Xi-1 + b2*Xi-2 + ... + bk*Xi-k) mod m
# Aqui ya no se usa un solo numero anterior, sino los ultimos k numeros.
# ===================================================================
class PseudoRandomMultipleCongruentialGenerator:

    def __init__(self, modulo, multiplicadores, semilla):
        # Paso 1: guardamos los datos del generador
        self.modulo = modulo                      # esto se usa para el "mod m"
        self.multiplicadores = multiplicadores     # esta es la lista [b1, b2, ..., bk]
        # la semilla es una lista con los primeros k valores: [X0, X1, ..., Xk-1]
        # el ultimo de la lista siempre es el valor mas reciente
        self.estado = list(semilla)

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            siguiente = 0

            # aqui sumamos b1*Xi-1 + b2*Xi-2 + ... recorriendo el estado de atras hacia adelante
            for j in range(len(self.multiplicadores)):
                siguiente = siguiente + self.multiplicadores[j] * self.estado[-1 - j]

            siguiente = siguiente % self.modulo

            # aqui "recorremos" el estado: quitamos el valor mas viejo y agregamos el nuevo
            self.estado.append(siguiente)
            self.estado.pop(0)

            muestra.append(siguiente / self.modulo)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo desde el estado actual
        estado = list(self.estado)   # copia del estado, para no modificar self.estado
        visitados = [list(estado)]   # aqui guardamos cada lista de estado que ya salio

        while True:
            siguiente = 0

            for j in range(len(self.multiplicadores)):
                siguiente = siguiente + self.multiplicadores[j] * estado[-1 - j]

            siguiente = siguiente % self.modulo
            estado = estado[1:] + [siguiente]

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(list(estado))


# --- ejemplo de uso ---
multiplicadores = [3, 5]
semilla = [1, 2]
ejemplo = PseudoRandomMultipleCongruentialGenerator(15, multiplicadores, semilla)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# ===================================================================
# 5. GENERADOR MULTIPLICATIVO COMBINADO
# Aqui se juntan DOS generadores multiplicativos simples (cada uno con
# su propio modulo y multiplicador) para armar uno solo mas fuerte:
#   Xi = (b1 * Xi-1) mod m1
#   Yi = (b2 * Yi-1) mod m2
#   Zi = (Xi - Yi) mod m1
#   Ui = Zi / m1
# ===================================================================
class PseudoRandomCombinedCongruentialGenerator:

    def __init__(self, modulo1, modulo2, multiplicador1, multiplicador2, semilla1, semilla2):
        # Paso 1: guardamos los datos de los DOS generadores
        self.modulo1 = modulo1               # modulo del primer generador (Xi)
        self.modulo2 = modulo2               # modulo del segundo generador (Yi)
        self.multiplicador1 = multiplicador1  # multiplicador del primer generador
        self.multiplicador2 = multiplicador2  # multiplicador del segundo generador
        self.x = semilla1                     # X0
        self.y = semilla2                     # Y0

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            # aqui avanzamos cada generador por separado
            self.x = (self.multiplicador1 * self.x) % self.modulo1
            self.y = (self.multiplicador2 * self.y) % self.modulo2

            # aqui los combinamos: Zi = (Xi - Yi) mod m1
            z = (self.x - self.y) % self.modulo1
            muestra.append(z / self.modulo1)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo revisando la pareja (Xi, Yi)
        x = self.x
        y = self.y
        visitados = [[x, y]]   # aqui guardamos cada pareja [Xi, Yi] que ya salio

        while True:
            x = (self.multiplicador1 * x) % self.modulo1
            y = (self.multiplicador2 * y) % self.modulo2

            if [x, y] in visitados:
                return len(visitados) - visitados.index([x, y])

            visitados.append([x, y])


# --- ejemplo de uso ---
ejemplo = PseudoRandomCombinedCongruentialGenerator(32, 31, 5, 3, 7, 11)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# ===================================================================
# 6. GENERADOR MULTIPLICATIVO MULTIPLE COMBINADO
# Esto es la union de los dos anteriores: en vez de combinar dos
# generadores simples, combinamos dos generadores MULTIPLES.
#   Xi = (b1*Xi-1 + ... + bk*Xi-k) mod m1
#   Yi = (c1*Yi-1 + ... + ck*Yi-k) mod m2
#   Zi = (Xi - Yi) mod m1
#   Ui = Zi / m1
# ===================================================================
class PseudoRandomMultipleCombinedCongruentialGenerator:

    def __init__(self, modulo1, modulo2, multiplicadores1, multiplicadores2, semilla1, semilla2):
        # Paso 1: guardamos los datos de los DOS generadores multiples
        self.modulo1 = modulo1                     # modulo del primer generador (rama X)
        self.modulo2 = modulo2                     # modulo del segundo generador (rama Y)
        self.multiplicadores1 = multiplicadores1   # lista [b1, ..., bk] de la rama X
        self.multiplicadores2 = multiplicadores2   # lista [c1, ..., ck] de la rama Y
        self.x = list(semilla1)   # semilla de la rama X: [X0, X1, ..., Xk-1]
        self.y = list(semilla2)   # semilla de la rama Y: [Y0, Y1, ..., Yk-1]

    def rand(self, n):
        # Paso 2: generamos n numeros pseudoaleatorios
        muestra = []

        for i in range(n):
            # aqui avanzamos la rama X, igual que en el generador multiple
            sx = 0
            for j in range(len(self.multiplicadores1)):
                sx = sx + self.multiplicadores1[j] * self.x[-1 - j]
            sx = sx % self.modulo1
            self.x.append(sx)
            self.x.pop(0)

            # aqui avanzamos la rama Y, igual que en el generador multiple
            sy = 0
            for j in range(len(self.multiplicadores2)):
                sy = sy + self.multiplicadores2[j] * self.y[-1 - j]
            sy = sy % self.modulo2
            self.y.append(sy)
            self.y.pop(0)

            # aqui combinamos las dos ramas: Zi = (Xi - Yi) mod m1
            z = (sx - sy) % self.modulo1
            muestra.append(z / self.modulo1)

        return muestra

    def periodo(self):
        # Paso 3: calculamos el periodo revisando la pareja de estados (rama X, rama Y)
        x = list(self.x)
        y = list(self.y)
        visitados = [[list(x), list(y)]]   # aqui guardamos cada pareja de estados que ya salio

        while True:
            sx = 0
            for j in range(len(self.multiplicadores1)):
                sx = sx + self.multiplicadores1[j] * x[-1 - j]
            sx = sx % self.modulo1
            x = x[1:] + [sx]

            sy = 0
            for j in range(len(self.multiplicadores2)):
                sy = sy + self.multiplicadores2[j] * y[-1 - j]
            sy = sy % self.modulo2
            y = y[1:] + [sy]

            estado = [list(x), list(y)]

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


# --- ejemplo de uso ---
multiplicadores1 = [3, 5]
multiplicadores2 = [2, 7]
semilla1 = [1, 2]
semilla2 = [4, 3]
ejemplo = PseudoRandomMultipleCombinedCongruentialGenerator(15, 13, multiplicadores1, multiplicadores2, semilla1, semilla2)
print(ejemplo.rand(10))
print(ejemplo.periodo())
