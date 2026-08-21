# ===================================================================
# GENERADORES DE NUMEROS PSEUDOALEATORIOS - VALORES YA DEFINIDOS
# ===================================================================
# Este archivo tiene la misma logica que "Generadores simples.py"
# (__init__ / rand(n) / periodo()), pero aqui ya no se dejan valores
# de ejemplo cualquiera: cada modulo, multiplicador, semilla o lista
# de coeficientes ya viene elegida, para que el codigo corra solo y
# se vea el resultado de una vez.
#
# Cada uno de los 6 codigos esta separado con una linea de guiones y
# un titulo, para que se identifique rapido donde empieza y donde
# termina cada generador.
# ===================================================================


# ###################################################################
# CODIGO 1 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO
# Formula:  X(i+1) = (b * Xi) mod m
# ###################################################################
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
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


# Valores elegidos: m = 31 (numero primo) y b = 3.
# 3 es raiz primitiva de 31, entonces el periodo queda al maximo posible: m - 1 = 30.
print("CODIGO 1 - Generador Multiplicativo")
ejemplo1 = PseudoRandomMultiplicativeCongruentialGenerator(modulo=31, multiplicador=3, semilla=1)
print(ejemplo1.rand(10))
print("periodo:", ejemplo1.periodo())
print()


# ###################################################################
# CODIGO 2 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO LINEAL
# Formula:  X(i+1) = (b * Xi + c) mod m
# ###################################################################
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


# Valores elegidos: m = 16, b = 5, c = 3.
# Cumplen las condiciones de Hull-Dobell (c y m primos entre si, b-1 multiplo
# de 4 porque m es multiplo de 4), asi que da periodo completo: m = 16.
print("CODIGO 2 - Generador Multiplicativo Lineal")
ejemplo2 = PseudoRandomLinearCongruentialGenerator(modulo=16, multiplicador=5, constante=3, semilla=7)
print(ejemplo2.rand(10))
print("periodo:", ejemplo2.periodo())
print()


# ###################################################################
# CODIGO 3 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO POLINOMIAL
# Formula:  X(i+1) = (b0 + b1*Xi + b2*Xi^2 + ... + bk*Xi^k) mod m
# ###################################################################
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


# Valores elegidos: m = 31, coeficientes = [3, 2, 5] (b0=3, b1=2, b2=5), semilla = 7.
print("CODIGO 3 - Generador Multiplicativo Polinomial")
ejemplo3 = PseudoRandomPolynomialCongruentialGenerator(modulo=31, coeficientes=[3, 2, 5], semilla=7)
print(ejemplo3.rand(10))
print("periodo:", ejemplo3.periodo())
print()


# ###################################################################
# CODIGO 4 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO MULTIPLE
# Formula:  Xi = (b1*Xi-1 + b2*Xi-2 + ... + bk*Xi-k) mod m
# ###################################################################
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


# Valores elegidos: m = 13, multiplicadores = [1, 1], semilla = [1, 1].
# Con b1=b2=1 queda igual que una sucesion de Fibonacci pero en modulo 13:
# como se usan los dos valores anteriores, el periodo (28) sale mas grande
# que el modulo, cosa que en el generador simple no pasa.
print("CODIGO 4 - Generador Multiplicativo Multiple")
ejemplo4 = PseudoRandomMultipleCongruentialGenerator(modulo=13, multiplicadores=[1, 1], semilla=[1, 1])
print(ejemplo4.rand(10))
print("periodo:", ejemplo4.periodo())
print()


# ###################################################################
# CODIGO 5 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO COMBINADO
# Xi = (b1 * Xi-1) mod m1  ,  Yi = (b2 * Yi-1) mod m2
# Zi = (Xi - Yi) mod m1    ,  Ui = Zi / m1
# ###################################################################
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


# Valores elegidos: primer generador m1=31, b1=3, X0=1 (solo, periodo 30);
# segundo generador m2=29, b2=2, Y0=1 (solo, periodo 28).
# Al combinarlos el periodo sube a 420, mas grande que los dos periodos
# por separado: esa es la ventaja de combinar dos generadores.
print("CODIGO 5 - Generador Multiplicativo Combinado")
ejemplo5 = PseudoRandomCombinedCongruentialGenerator(
    modulo1=31, modulo2=29,
    multiplicador1=3, multiplicador2=2,
    semilla1=1, semilla2=1
)
print(ejemplo5.rand(10))
print("periodo:", ejemplo5.periodo())
print()


# ###################################################################
# CODIGO 6 --------------------------------------------------------
# GENERADOR MULTIPLICATIVO MULTIPLE COMBINADO
# Xi = (b1*Xi-1 + ... + bk*Xi-k) mod m1
# Yi = (c1*Yi-1 + ... + ck*Yi-k) mod m2
# Zi = (Xi - Yi) mod m1  ,  Ui = Zi / m1
# ###################################################################
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


# Valores elegidos: rama X con m1=31, multiplicadores=[1, 1], semilla=[1, 2];
# rama Y con m2=29, multiplicadores=[2, 3], semilla=[1, 1].
# Cada rama sola ya tiene un periodo largo (30 y 28), y al combinarlas el
# periodo final vuelve a subir a 420.
print("CODIGO 6 - Generador Multiplicativo Multiple Combinado")
ejemplo6 = PseudoRandomMultipleCombinedCongruentialGenerator(
    modulo1=31, modulo2=29,
    multiplicadores1=[1, 1], multiplicadores2=[2, 3],
    semilla1=[1, 2], semilla2=[1, 1]
)
print(ejemplo6.rand(10))
print("periodo:", ejemplo6.periodo())
