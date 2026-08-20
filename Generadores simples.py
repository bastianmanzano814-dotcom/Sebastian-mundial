# 1. Generador Multiplicativo
# Xi+1 = (b * Xi) mod m
class PseudoRandomMultiplicativeCongruentialGenerator:
    def __init__(self, modulo, multiplicador, semilla):
        self.modulo = modulo
        self.multiplicador = multiplicador
        self.estado = semilla

    def rand(self, n):
        muestra = []

        for i in range(n):
            self.estado = (self.multiplicador * self.estado) % self.modulo
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        estado = self.estado
        visitados = [estado]

        while True:
            estado = (self.multiplicador * estado) % self.modulo

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


ejemplo = PseudoRandomMultiplicativeCongruentialGenerator(15, 7, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# 2. Generador Multiplicativo Lineal
# Xi+1 = (b * Xi + c) mod m
class PseudoRandomLinearCongruentialGenerator:
    def __init__(self, modulo, multiplicador, constante, semilla):
        self.modulo = modulo
        self.multiplicador = multiplicador
        self.constante = constante
        self.estado = semilla

    def rand(self, n):
        muestra = []

        for i in range(n):
            self.estado = (self.multiplicador * self.estado + self.constante) % self.modulo
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        estado = self.estado
        visitados = [estado]

        while True:
            estado = (self.multiplicador * estado + self.constante) % self.modulo

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


ejemplo = PseudoRandomLinearCongruentialGenerator(28, 7, 3, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# 3. Generador Multiplicativo Polinomial
# Xi+1 = (b0 + b1*Xi + b2*Xi^2 + ... + bk*Xi^k) mod m
class PseudoRandomPolynomialCongruentialGenerator:
    def __init__(self, modulo, coeficientes, semilla):
        self.modulo = modulo
        self.coeficientes = coeficientes
        self.estado = semilla

    def polinomio(self, x):
        resultado = 0

        for i in range(len(self.coeficientes)):
            resultado = resultado + self.coeficientes[i] * x**i

        return resultado

    def rand(self, n):
        muestra = []

        for i in range(n):
            self.estado = self.polinomio(self.estado) % self.modulo
            muestra.append(self.estado / self.modulo)

        return muestra

    def periodo(self):
        estado = self.estado
        visitados = [estado]

        while True:
            estado = self.polinomio(estado) % self.modulo

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(estado)


coef = [3, 2, 5]
ejemplo = PseudoRandomPolynomialCongruentialGenerator(28, coef, 7)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# 4. Generador Multiplicativo Multiple
# Xi = (b1*Xi-1 + b2*Xi-2 + ... + bk*Xi-k) mod m
class PseudoRandomMultipleCongruentialGenerator:
    def __init__(self, modulo, multiplicadores, semilla):
        self.modulo = modulo
        self.multiplicadores = multiplicadores
        self.estado = list(semilla)

    def rand(self, n):
        muestra = []

        for i in range(n):
            siguiente = 0

            for j in range(len(self.multiplicadores)):
                siguiente = siguiente + self.multiplicadores[j] * self.estado[-1 - j]

            siguiente = siguiente % self.modulo

            self.estado.append(siguiente)
            self.estado.pop(0)

            muestra.append(siguiente / self.modulo)

        return muestra

    def periodo(self):
        estado = list(self.estado)
        visitados = [list(estado)]

        while True:
            siguiente = 0

            for j in range(len(self.multiplicadores)):
                siguiente = siguiente + self.multiplicadores[j] * estado[-1 - j]

            siguiente = siguiente % self.modulo
            estado = estado[1:] + [siguiente]

            if estado in visitados:
                return len(visitados) - visitados.index(estado)

            visitados.append(list(estado))


multiplicadores = [3, 5]
semilla = [1, 2]
ejemplo = PseudoRandomMultipleCongruentialGenerator(15, multiplicadores, semilla)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# 5. Generador Multiplicativo Combinado
# Xi = (b1*Xi-1) mod m1 ,  Yi = (b2*Yi-1) mod m2
# Zi = (Xi - Yi) mod m1  ,  Ui = Zi / m1
class PseudoRandomCombinedCongruentialGenerator:
    def __init__(self, modulo1, modulo2, multiplicador1, multiplicador2, semilla1, semilla2):
        self.modulo1 = modulo1
        self.modulo2 = modulo2
        self.multiplicador1 = multiplicador1
        self.multiplicador2 = multiplicador2
        self.x = semilla1
        self.y = semilla2

    def rand(self, n):
        muestra = []

        for i in range(n):
            self.x = (self.multiplicador1 * self.x) % self.modulo1
            self.y = (self.multiplicador2 * self.y) % self.modulo2

            z = (self.x - self.y) % self.modulo1
            muestra.append(z / self.modulo1)

        return muestra

    def periodo(self):
        x = self.x
        y = self.y
        visitados = [[x, y]]

        while True:
            x = (self.multiplicador1 * x) % self.modulo1
            y = (self.multiplicador2 * y) % self.modulo2

            if [x, y] in visitados:
                return len(visitados) - visitados.index([x, y])

            visitados.append([x, y])


ejemplo = PseudoRandomCombinedCongruentialGenerator(32, 31, 5, 3, 7, 11)
print(ejemplo.rand(10))
print(ejemplo.periodo())


# 6. Generador Multiplicativo Multiple Combinado
# Xi = (b1*Xi-1 + ... + bk*Xi-k) mod m1
# Yi = (c1*Yi-1 + ... + ck*Yi-k) mod m2
# Zi = (Xi - Yi) mod m1  ,  Ui = Zi / m1
class PseudoRandomMultipleCombinedCongruentialGenerator:
    def __init__(self, modulo1, modulo2, multiplicadores1, multiplicadores2, semilla1, semilla2):
        self.modulo1 = modulo1
        self.modulo2 = modulo2
        self.multiplicadores1 = multiplicadores1
        self.multiplicadores2 = multiplicadores2
        self.x = list(semilla1)
        self.y = list(semilla2)

    def rand(self, n):
        muestra = []

        for i in range(n):
            sx = 0

            for j in range(len(self.multiplicadores1)):
                sx = sx + self.multiplicadores1[j] * self.x[-1 - j]

            sx = sx % self.modulo1
            self.x.append(sx)
            self.x.pop(0)

            sy = 0

            for j in range(len(self.multiplicadores2)):
                sy = sy + self.multiplicadores2[j] * self.y[-1 - j]

            sy = sy % self.modulo2
            self.y.append(sy)
            self.y.pop(0)

            z = (sx - sy) % self.modulo1
            muestra.append(z / self.modulo1)

        return muestra

    def periodo(self):
        x = list(self.x)
        y = list(self.y)
        visitados = [[list(x), list(y)]]

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


multiplicadores1 = [3, 5]
multiplicadores2 = [2, 7]
semilla1 = [1, 2]
semilla2 = [4, 3]
ejemplo = PseudoRandomMultipleCombinedCongruentialGenerator(15, 13, multiplicadores1, multiplicadores2, semilla1, semilla2)
print(ejemplo.rand(10))
print(ejemplo.periodo())
