# ============================================================
#  PREDICTOR DE MARCADORES DE FÚTBOL
#  Modelo estadístico: Regresión + Proceso de Poisson +
#  Probabilidad Multivariada + Correlaciones
# ============================================================
#
# AUTOR    : Análisis Estadístico Deportivo
# VERSIÓN  : 1.0
# LENGUAJE : R (compatible con RStudio)
#
# ── FORMATO DE DATOS REQUERIDO ──────────────────────────────
#
# El archivo CSV o XLSX debe tener las siguientes columnas:
#
#  OBLIGATORIAS:
#   equipo          → Nombre del equipo
#   rival           → Nombre del equipo contrario
#   local_visitante → "Local" o "Visitante"
#   goles_favor     → Goles anotados en ese partido
#   goles_contra    → Goles recibidos en ese partido
#   resultado       → "Victoria", "Empate" o "Derrota"
#
#  RECOMENDADAS:
#   fecha           → Fecha del partido (YYYY-MM-DD)
#   tipo_partido    → "Liga", "Copa", "Eliminatoria", "Amistoso"
#   clima           → "Soleado", "Nublado", "Lluvia", "Frio"
#   temperatura     → Temperatura en grados Celsius
#   humedad         → Porcentaje de humedad (0-100)
#   dias_descanso   → Días desde el último partido
#
#  OPCIONALES (el modelo funciona sin ellas):
#   tiros_puerta    → Tiros a puerta en ese partido
#   posesion        → Porcentaje de posesión (0-100)
#   lesionados      → Número de jugadores lesionados
#   suspendidos     → Número de jugadores suspendidos
#
# ── EJEMPLO DE DATOS (primeras filas del CSV) ───────────────
#
#  equipo,rival,fecha,local_visitante,goles_favor,goles_contra,resultado,tipo_partido,clima,temperatura,humedad,dias_descanso
#  Real Madrid,Barcelona,2024-10-26,Local,3,1,Victoria,Liga,Soleado,22,55,7
#  Real Madrid,Atletico,2024-10-19,Visitante,1,1,Empate,Liga,Nublado,18,65,7
#  ...
#
# ============================================================


# ============================================================
# BLOQUE 1 — INSTALACIÓN Y CARGA DE LIBRERÍAS
# ============================================================
# Propósito: cargar las herramientas estadísticas y gráficas
# necesarias. Si alguna no está instalada, se instala sola.
#
# Librerías utilizadas:
#   readxl    → Leer archivos Excel (.xlsx)
#   ggplot2   → Gráficas avanzadas de alta calidad
#   corrplot  → Visualización de matrices de correlación
#   dplyr     → Manipulación y filtrado de datos
#   reshape2  → Transformar matrices en tablas para ggplot2
#   gridExtra → Organizar múltiples gráficas en una ventana
#   scales    → Formato de ejes en gráficas

paquetes <- c("readxl", "ggplot2", "corrplot", "dplyr",
              "reshape2", "gridExtra", "scales", "RColorBrewer")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("       PREDICTOR DE MARCADORES DE FÚTBOL\n")
cat("       Modelo: Regresión + Poisson + Probabilidad\n")
cat("═══════════════════════════════════════════════════════════\n\n")


# ============================================================
# BLOQUE 2 — PARÁMETROS DEL PARTIDO A PREDECIR
# ============================================================
# Propósito: el usuario define aquí el partido específico
# que desea predecir. Modifique únicamente estos valores.
#
# Todos los demás cálculos se harán automáticamente.

equipo_local             <- "Real Madrid"  # Nombre del equipo local
equipo_visitante         <- "Barcelona"    # Nombre del equipo visitante
clima_partido            <- "Soleado"      # "Soleado", "Nublado", "Lluvia", "Frio"
temperatura_partido      <- 22             # Grados Celsius
humedad_partido          <- 55             # Porcentaje (0-100)
tipo_partido             <- "Liga"         # "Liga", "Copa", "Eliminatoria", "Amistoso"
dias_descanso_local      <- 7             # Días de descanso del equipo local
dias_descanso_visitante  <- 5             # Días de descanso del equipo visitante
n_simulaciones           <- 10000         # Número de simulaciones Monte Carlo
max_partidos_analizar    <- 10            # Últimos N partidos a usar por equipo

cat("───────────────────────────────────────────────────────────\n")
cat("  PARTIDO A PREDECIR:\n")
cat(sprintf("    Local     : %s\n", equipo_local))
cat(sprintf("    Visitante : %s\n", equipo_visitante))
cat(sprintf("    Tipo      : %s\n", tipo_partido))
cat(sprintf("    Clima     : %s | %.0f°C | Humedad: %.0f%%\n",
            clima_partido, temperatura_partido, humedad_partido))
cat(sprintf("    Descanso  : Local %dd | Visitante %dd\n",
            dias_descanso_local, dias_descanso_visitante))
cat(sprintf("    Simulaciones: %s\n", format(n_simulaciones, big.mark = ",")))
cat("───────────────────────────────────────────────────────────\n\n")


# ============================================================
# BLOQUE 3 — CARGA DE DATOS
# ============================================================
# Propósito: el usuario selecciona el archivo de datos históricos
# desde el explorador de archivos de su computadora.
# Se acepta formato CSV (.csv) y Excel (.xlsx / .xls).
#
# file.choose() abre un diálogo para seleccionar el archivo.
# El separador del CSV puede ser coma (,) o punto y coma (;).

cat(">>> Por favor seleccione el archivo de datos históricos...\n")
cat("    (Formatos aceptados: .csv, .xlsx, .xls)\n\n")

archivo   <- file.choose()
extension <- tolower(tools::file_ext(archivo))

if (extension == "csv") {
  # Intentar con coma y si falla, con punto y coma
  datos <- tryCatch(
    read.csv(archivo, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) read.csv2(archivo, stringsAsFactors = FALSE, encoding = "UTF-8")
  )
  cat("  Archivo CSV cargado correctamente.\n")
} else if (extension %in% c("xlsx", "xls")) {
  datos <- as.data.frame(readxl::read_excel(archivo))
  cat("  Archivo Excel cargado correctamente.\n")
} else {
  stop("Formato no soportado. Use .csv o .xlsx")
}

cat(sprintf("  Registros cargados : %d filas × %d columnas\n\n",
            nrow(datos), ncol(datos)))

cat("  Columnas detectadas:\n")
cat(paste("   ", paste(names(datos), collapse = ", ")), "\n\n")


# ============================================================
# BLOQUE 4 — LIMPIEZA Y PREPARACIÓN DE DATOS
# ============================================================
# Propósito: estandarizar los datos para que el modelo funcione
# correctamente, independientemente de cómo los haya escrito
# el usuario en el archivo.
#
# Pasos:
#   1. Estandarizar nombres de columnas
#   2. Reportar y tratar valores faltantes (NA)
#   3. Convertir tipos de variables
#   4. Crear variables derivadas necesarias para el modelo
#   5. Agregar columnas opcionales si no existen

# ── 4.1  Estandarizar nombres (minúsculas, sin espacios) ──
names(datos) <- tolower(trimws(gsub("[^a-zA-Z0-9_]", "_", names(datos))))

# ── 4.2  Verificar columnas obligatorias ──────────────────
cols_obligatorias <- c("equipo", "goles_favor", "goles_contra",
                       "local_visitante", "resultado")
cols_faltantes <- cols_obligatorias[!cols_obligatorias %in% names(datos)]
if (length(cols_faltantes) > 0) {
  stop(paste(
    "ERROR: Columnas obligatorias no encontradas en el archivo:\n  ",
    paste(cols_faltantes, collapse = ", "),
    "\n\nRevise el formato de datos descrito al inicio del script."
  ))
}

# ── 4.3  Convertir fecha ──────────────────────────────────
if ("fecha" %in% names(datos)) {
  datos$fecha <- suppressWarnings(as.Date(datos$fecha))
}

# ── 4.4  Revisar y tratar valores faltantes ───────────────
na_total <- sum(is.na(datos))
if (na_total > 0) {
  cat(sprintf("  AVISO: Se encontraron %d valores faltantes (NA).\n", na_total))
  cat("  Distribución por columna:\n")
  na_col <- colSums(is.na(datos))
  print(na_col[na_col > 0])
  cat("  Imputando: numéricos → mediana, texto → categoría más frecuente.\n\n")

  for (col in names(datos)) {
    if (is.numeric(datos[[col]]) && any(is.na(datos[[col]]))) {
      datos[[col]][is.na(datos[[col]])] <- median(datos[[col]], na.rm = TRUE)
    } else if (is.character(datos[[col]]) && any(is.na(datos[[col]]))) {
      moda <- names(sort(table(datos[[col]]), decreasing = TRUE))[1]
      datos[[col]][is.na(datos[[col]])] <- moda
    }
  }
} else {
  cat("  Sin valores faltantes. Datos completos.\n\n")
}

# ── 4.5  Estandarizar texto en columnas clave ─────────────
estandarizar <- function(x) trimws(tolower(as.character(x)))

datos$local_visitante <- trimws(tolower(datos$local_visitante))
datos$resultado       <- trimws(tolower(datos$resultado))
if ("tipo_partido" %in% names(datos))
  datos$tipo_partido <- trimws(tolower(datos$tipo_partido))
if ("clima" %in% names(datos))
  datos$clima <- trimws(tolower(datos$clima))

# ── 4.6  Variables numéricas derivadas ───────────────────
datos$es_local   <- as.integer(datos$local_visitante == "local")
datos$dif_goles  <- datos$goles_favor - datos$goles_contra
datos$puntos     <- ifelse(datos$resultado == "victoria", 3L,
                   ifelse(datos$resultado == "empate", 1L, 0L))
datos$es_oficial <- if ("tipo_partido" %in% names(datos))
  as.integer(datos$tipo_partido != "amistoso") else 1L

# ── 4.7  Columnas opcionales: crear con 0 si no existen ──
vars_opcionales <- c("tiros_puerta", "posesion", "lesionados",
                     "suspendidos", "temperatura", "humedad", "dias_descanso")
for (v in vars_opcionales) {
  if (!v %in% names(datos)) {
    datos[[v]] <- 0
    cat(sprintf("  NOTA: Columna '%s' no encontrada. Se usará 0.\n", v))
  }
}

# Asegurar que las numéricas sean numéricas
num_cols <- c("goles_favor", "goles_contra", "dif_goles", "puntos",
              "temperatura", "humedad", "dias_descanso", "tiros_puerta",
              "posesion", "lesionados", "suspendidos")
for (col in intersect(num_cols, names(datos))) {
  datos[[col]] <- suppressWarnings(as.numeric(datos[[col]]))
  datos[[col]][is.na(datos[[col]])] <- 0
}

cat("\n  Vista previa de datos limpios:\n")
cols_preview <- intersect(
  c("equipo", "rival", "local_visitante", "goles_favor", "goles_contra",
    "resultado", "tipo_partido", "clima"), names(datos))
print(head(datos[, cols_preview], 5))
cat("\n")


# ============================================================
# BLOQUE 5 — FILTRADO DE DATOS POR EQUIPO
# ============================================================
# Propósito: extraer los últimos N partidos de cada equipo
# para el análisis. Si hay columna de fecha, se ordena por ella;
# si no, se toma el orden del archivo (se asume orden cronológico
# inverso, el primero es el más reciente).

obtener_partidos <- function(datos, nombre_equipo, n = 10) {
  df <- datos[tolower(datos$equipo) == tolower(nombre_equipo), ]
  if (nrow(df) == 0) {
    stop(sprintf(
      "No se encontraron partidos para '%s'.\nEquipos disponibles: %s",
      nombre_equipo, paste(unique(datos$equipo), collapse = ", ")
    ))
  }
  if ("fecha" %in% names(df) && !all(is.na(df$fecha))) {
    df <- df[order(df$fecha, decreasing = TRUE), ]
  }
  head(df, n)
}

datos_local     <- obtener_partidos(datos, equipo_local,     max_partidos_analizar)
datos_visitante <- obtener_partidos(datos, equipo_visitante, max_partidos_analizar)

cat(sprintf("  Partidos para %-20s: %d\n", equipo_local,     nrow(datos_local)))
cat(sprintf("  Partidos para %-20s: %d\n", equipo_visitante, nrow(datos_visitante)))
cat("\n")


# ============================================================
# BLOQUE 6 — ESTADÍSTICAS DESCRIPTIVAS POR EQUIPO
# ============================================================
# Propósito: calcular todos los indicadores de rendimiento
# de cada equipo que luego se usarán como variables explicativas
# en el modelo de regresión y para calcular los lambdas de Poisson.
#
# Variables calculadas:
#   • Promedios de goles (global, como local, visitante)
#   • Rendimiento en condiciones climáticas similares
#   • Forma reciente (últimos 5 partidos en puntos)
#   • Número de victorias, empates, derrotas
#   • Racha actual (tipo y duración)
#   • Rendimiento ofensivo y defensivo (índice 0-1)
#   • Factor de fatiga según días de descanso

calcular_estadisticas <- function(df, nombre, dias_descanso_param) {
  s <- list(nombre = nombre, n = nrow(df))

  # Promedios globales
  s$gf_prom  <- mean(df$goles_favor,  na.rm = TRUE)
  s$gc_prom  <- mean(df$goles_contra, na.rm = TRUE)
  s$dg_prom  <- mean(df$dif_goles,    na.rm = TRUE)

  # Resultados
  s$victorias <- sum(df$resultado == "victoria")
  s$empates   <- sum(df$resultado == "empate")
  s$derrotas  <- sum(df$resultado == "derrota")

  # Forma reciente: suma de puntos en los últimos 5 partidos (máx 15)
  ult5          <- head(df, 5)
  s$forma_pts   <- sum(ult5$puntos)
  s$forma_pct   <- s$forma_pts / 15   # normalizado 0-1

  # Rendimiento como Local
  local_df       <- df[df$es_local == 1, ]
  s$gf_local     <- if (nrow(local_df) > 0) mean(local_df$goles_favor)  else s$gf_prom
  s$gc_local     <- if (nrow(local_df) > 0) mean(local_df$goles_contra) else s$gc_prom

  # Rendimiento como Visitante
  vis_df         <- df[df$es_local == 0, ]
  s$gf_visit     <- if (nrow(vis_df) > 0) mean(vis_df$goles_favor)  else s$gf_prom
  s$gc_visit     <- if (nrow(vis_df) > 0) mean(vis_df$goles_contra) else s$gc_prom

  # Rendimiento en partidos oficiales
  of_df          <- df[df$es_oficial == 1, ]
  s$gf_oficial   <- if (nrow(of_df) > 0) mean(of_df$goles_favor) else s$gf_prom

  # Rendimiento en clima similar al partido
  if ("clima" %in% names(df)) {
    clim_df      <- df[tolower(df$clima) == tolower(clima_partido), ]
    s$gf_clima   <- if (nrow(clim_df) > 0) mean(clim_df$goles_favor)  else s$gf_prom
    s$gc_clima   <- if (nrow(clim_df) > 0) mean(clim_df$goles_contra) else s$gc_prom
  } else {
    s$gf_clima   <- s$gf_prom
    s$gc_clima   <- s$gc_prom
  }

  # Racha actual (secuencia de resultados iguales desde el más reciente)
  res_vec  <- df$resultado
  s$racha_tipo <- res_vec[1]
  s$racha_n    <- 1L
  if (length(res_vec) > 1) {
    for (i in 2:length(res_vec)) {
      if (res_vec[i] == s$racha_tipo) s$racha_n <- s$racha_n + 1L else break
    }
  }

  # Índices de rendimiento ofensivo y defensivo (0 = muy malo, 1 = muy bueno)
  s$rend_ofensivo  <- min(s$gf_prom / 3, 1)
  s$rend_defensivo <- 1 - min(s$gc_prom / 3, 1)

  # Factor de fatiga (penalización por pocos días de descanso)
  s$dias_descanso <- dias_descanso_param
  s$fatiga <- if (dias_descanso_param < 3) 0.85 else
              if (dias_descanso_param < 5) 0.93 else 1.00

  s
}

sL <- calcular_estadisticas(datos_local,     equipo_local,     dias_descanso_local)
sV <- calcular_estadisticas(datos_visitante, equipo_visitante, dias_descanso_visitante)

# Imprimir resumen de estadísticas
resumen_equipo <- function(s, rol) {
  cat(sprintf("\n  ── %s (%s) ─────────────────────────────\n", toupper(s$nombre), rol))
  cat(sprintf("    Partidos analizados         : %d\n", s$n))
  cat(sprintf("    Goles anotados (promedio)   : %.2f\n", s$gf_prom))
  cat(sprintf("    Goles recibidos (promedio)  : %.2f\n", s$gc_prom))
  cat(sprintf("    Victorias / Empates / Derrotas : %d / %d / %d\n",
              s$victorias, s$empates, s$derrotas))
  cat(sprintf("    Forma reciente (pts/15)     : %d/15  (%.0f%%)\n",
              s$forma_pts, s$forma_pct * 100))
  cat(sprintf("    Racha actual                : %d %s\n", s$racha_n, s$racha_tipo))
  cat(sprintf("    Rendimiento ofensivo        : %.2f\n", s$rend_ofensivo))
  cat(sprintf("    Rendimiento defensivo       : %.2f\n", s$rend_defensivo))
  cat(sprintf("    Factor fatiga               : %.2f (descanso: %dd)\n",
              s$fatiga, s$dias_descanso))
}

cat("═══════════════════════════════════════════════════════════\n")
cat("  ESTADÍSTICAS DESCRIPTIVAS DE LOS EQUIPOS\n")
cat("═══════════════════════════════════════════════════════════")
resumen_equipo(sL, "LOCAL")
resumen_equipo(sV, "VISITANTE")
cat("\n")


# ============================================================
# BLOQUE 7 — ENFRENTAMIENTOS DIRECTOS (HEAD TO HEAD)
# ============================================================
# Propósito: analizar el historial entre ambos equipos
# para detectar patrones de goles en enfrentamientos directos.
# Este historial se incorpora como factor de ajuste en el lambda.

cat("═══════════════════════════════════════════════════════════\n")
cat("  HISTORIAL DE ENFRENTAMIENTOS DIRECTOS (H2H)\n")
cat("═══════════════════════════════════════════════════════════\n")

# Buscar partidos donde un equipo jugó contra el otro
h2h <- datos[
  (tolower(datos$equipo) == tolower(equipo_local) &
   tolower(datos$rival)  == tolower(equipo_visitante)) |
  (tolower(datos$equipo) == tolower(equipo_visitante) &
   tolower(datos$rival)  == tolower(equipo_local)),
]

if (nrow(h2h) > 0) {
  # Goles del local en H2H
  h2h_local <- h2h[tolower(h2h$equipo) == tolower(equipo_local), ]
  h2h_visit <- h2h[tolower(h2h$equipo) == tolower(equipo_visitante), ]

  h2h_gf_local <- if (nrow(h2h_local) > 0) mean(h2h_local$goles_favor) else sL$gf_prom
  h2h_gf_visit <- if (nrow(h2h_visit) > 0) mean(h2h_visit$goles_favor) else sV$gf_prom

  cat(sprintf("  Partidos H2H encontrados         : %d\n", nrow(h2h)))
  cat(sprintf("  Prom. goles %s en H2H  : %.2f\n", equipo_local, h2h_gf_local))
  cat(sprintf("  Prom. goles %s en H2H  : %.2f\n", equipo_visitante, h2h_gf_visit))
} else {
  cat("  No se encontraron enfrentamientos directos.\n")
  cat("  Se usarán estadísticas individuales de cada equipo.\n")
  h2h_gf_local <- sL$gf_prom
  h2h_gf_visit <- sV$gf_prom
}
cat("\n")


# ============================================================
# BLOQUE 8 — MODELO DE REGRESIÓN LINEAL
# ============================================================
# Propósito: construir un modelo estadístico formal para estimar
# la cantidad de goles esperados en función de las variables
# disponibles en el dataset.
#
# El modelo usa mínimos cuadrados ordinarios (OLS):
#   goles_favor = β0 + β1·es_local + β2·puntos + β3·temperatura + ...
#
# La variable dependiente es "goles_favor" (goles anotados).
# Las variables explicativas se seleccionan dinámicamente según
# las columnas disponibles en el dataset.
#
# Se muestra: coeficientes, R², residuos e interpretación.

cat("═══════════════════════════════════════════════════════════\n")
cat("  MODELO DE REGRESIÓN LINEAL\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Variables candidatas para la regresión
vars_candidatas <- c("es_local", "dif_goles", "puntos", "es_oficial",
                     "temperatura", "humedad", "dias_descanso",
                     "tiros_puerta", "posesion", "lesionados", "suspendidos")

vars_disponibles <- vars_candidatas[vars_candidatas %in% names(datos)]

# Construir dataset de regresión solo con columnas numéricas
datos_reg <- datos[, c("goles_favor", vars_disponibles), drop = FALSE]
datos_reg  <- datos_reg[complete.cases(datos_reg), ]

# Eliminar predictores con varianza cero (constantes no aportan)
vars_ok <- vars_disponibles[sapply(vars_disponibles, function(v) {
  x <- datos_reg[[v]]
  is.numeric(x) && var(x, na.rm = TRUE) > 1e-10
})]

if (length(vars_ok) == 0) {
  cat("  AVISO: Sin variables con varianza suficiente para regresión.\n")
  cat("  Se usará solo el intercepto (media global).\n\n")
  modelo_reg <- lm(goles_favor ~ 1, data = datos_reg)
} else {
  formula_reg <- as.formula(paste("goles_favor ~", paste(vars_ok, collapse = " + ")))
  modelo_reg  <- lm(formula_reg, data = datos_reg)
}

cat("  Resumen del modelo (variable dependiente: goles_favor):\n\n")
print(summary(modelo_reg))

# Interpretación automática de coeficientes
cat("\n  ── INTERPRETACIÓN DE COEFICIENTES ──────────────────────\n")
coefs <- summary(modelo_reg)$coefficients
for (i in seq_len(nrow(coefs))) {
  nombre  <- rownames(coefs)[i]
  beta    <- coefs[i, 1]
  p_val   <- coefs[i, 4]
  if (nombre == "(Intercept)") {
    cat(sprintf("  • Intercepto: En condiciones base, se esperan %.2f goles.\n", beta))
  } else if (p_val < 0.10) {
    sig   <- if (p_val < 0.01) "(***)" else if (p_val < 0.05) "(**)" else "(*)"
    dir   <- if (beta > 0) "AUMENTA" else "DISMINUYE"
    cat(sprintf("  • '%s': %s los goles en %.3f por unidad %s\n",
                nombre, dir, abs(beta), sig))
  }
}
r2 <- summary(modelo_reg)$r.squared
cat(sprintf("\n  R² = %.4f  →  El modelo explica el %.1f%% de la variabilidad en goles.\n",
            r2, r2 * 100))
cat("\n")


# ============================================================
# BLOQUE 9 — CÁLCULO DE GOLES ESPERADOS (LAMBDA DE POISSON)
# ============================================================
# Propósito: calcular el parámetro λ (lambda) de la distribución
# de Poisson para cada equipo. Lambda representa la tasa media
# de goles esperados en el partido.
#
# Se construye como una media ponderada que combina:
#   • Rendimiento propio (goles como local/visitante)
#   • Rendimiento en clima similar
#   • Historial H2H
#   • Rendimiento en partidos oficiales
#
# Luego se ajusta por factores multiplicativos:
#   • Forma reciente
#   • Nivel defensivo del rival
#   • Ventaja de localía (bonus)
#   • Condiciones climáticas
#   • Temperatura
#   • Humedad
#   • Tipo de partido (oficial vs amistoso)
#   • Fatiga

cat("═══════════════════════════════════════════════════════════\n")
cat("  CÁLCULO DE GOLES ESPERADOS — PROCESO DE POISSON\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# ── Factores de ajuste por condiciones externas ──────────
f_clima <- switch(tolower(clima_partido),
  "lluvia"  = 0.88,
  "frio"    = 0.92,
  "nublado" = 0.97,
  "soleado" = 1.00,
  1.00)

f_temp <- if (temperatura_partido < 5  | temperatura_partido > 35) 0.90 else
           if (temperatura_partido < 10 | temperatura_partido > 30) 0.95 else 1.00

f_hum  <- if (humedad_partido > 80) 0.93 else
           if (humedad_partido > 70) 0.97 else 1.00

f_tipo <- if (tolower(tipo_partido) == "amistoso") 0.90 else 1.00

# ── Lambda del equipo LOCAL ───────────────────────────────
# Base ponderada:
#   50% → rendimiento como local (más relevante)
#   20% → rendimiento en clima similar
#   20% → historial H2H
#   10% → rendimiento en partidos oficiales
base_lambda_L <- 0.50 * sL$gf_local   +
                 0.20 * sL$gf_clima    +
                 0.20 * h2h_gf_local   +
                 0.10 * sL$gf_oficial

# Factor forma reciente (rango 0.85-1.15)
f_forma_L <- 0.85 + 0.30 * sL$forma_pct

# Factor defensivo del rival: si el visitante es muy defensivo, reduce
f_def_V   <- 1 - (sV$rend_defensivo - 0.50) * 0.30

# Bonus de localía histórico en fútbol (~10-15% más goles como local)
bonus_localía <- 0.18

lambda_local <- (base_lambda_L + bonus_localía) *
                 f_forma_L   *
                 f_def_V     *
                 f_clima     *
                 f_temp      *
                 f_hum       *
                 f_tipo      *
                 sL$fatiga

lambda_local <- max(0.30, lambda_local)   # mínimo técnico

# ── Lambda del equipo VISITANTE ───────────────────────────
base_lambda_V <- 0.50 * sV$gf_visit   +
                 0.20 * sV$gf_clima    +
                 0.20 * h2h_gf_visit   +
                 0.10 * sV$gf_oficial

f_forma_V <- 0.85 + 0.30 * sV$forma_pct
f_def_L   <- 1 - (sL$rend_defensivo - 0.50) * 0.30

lambda_visitante <- base_lambda_V *
                    f_forma_V   *
                    f_def_L     *
                    f_clima     *
                    f_temp      *
                    f_hum       *
                    f_tipo      *
                    sV$fatiga

lambda_visitante <- max(0.30, lambda_visitante)

# Mostrar lambdas y factores
cat(sprintf("  λ (lambda) LOCAL     [%s]: %.4f goles/partido\n",
            equipo_local, lambda_local))
cat(sprintf("  λ (lambda) VISITANTE [%s]: %.4f goles/partido\n\n",
            equipo_visitante, lambda_visitante))

cat("  Factores de ajuste aplicados:\n")
cat(sprintf("    %-35s: %.3f\n", paste0("Clima (", clima_partido, ")"), f_clima))
cat(sprintf("    %-35s: %.3f\n", paste0("Temperatura (", temperatura_partido, "°C)"), f_temp))
cat(sprintf("    %-35s: %.3f\n", paste0("Humedad (", humedad_partido, "%)"), f_hum))
cat(sprintf("    %-35s: %.3f\n", paste0("Tipo partido (", tipo_partido, ")"), f_tipo))
cat(sprintf("    %-35s: %.3f\n", "Fatiga equipo local",     sL$fatiga))
cat(sprintf("    %-35s: %.3f\n", "Fatiga equipo visitante", sV$fatiga))
cat(sprintf("    %-35s: +%.3f\n", "Bonus ventaja de localía", bonus_localía))
cat("\n")


# ============================================================
# BLOQUE 10 — SIMULACIÓN MONTE CARLO (DISTRIBUCIÓN DE POISSON)
# ============================================================
# Propósito: simular miles de partidos posibles asumiendo que
# los goles de cada equipo siguen distribuciones de Poisson
# independientes, con los parámetros lambda calculados.
#
# X ~ Poisson(λ_local)      → goles del equipo local
# Y ~ Poisson(λ_visitante)  → goles del equipo visitante
#
# Con 10,000 simulaciones, cada marcador observado se convierte
# en una estimación de su probabilidad real.
#
# rpois(n, λ) genera n números aleatorios de la distribución Poisson.

cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  SIMULACIÓN MONTE CARLO — %s PARTIDOS\n",
            format(n_simulaciones, big.mark = ",")))
cat("═══════════════════════════════════════════════════════════\n\n")

set.seed(42)   # Semilla para reproducibilidad de resultados
goles_L_sim <- rpois(n_simulaciones, lambda_local)
goles_V_sim <- rpois(n_simulaciones, lambda_visitante)

# Construir tabla de marcadores
marcadores_vec   <- paste(goles_L_sim, goles_V_sim, sep = "-")
tabla_marcadores <- as.data.frame(table(marcadores_vec),
                                  stringsAsFactors = FALSE)
names(tabla_marcadores) <- c("Marcador", "Frecuencia")
tabla_marcadores$Probabilidad <- tabla_marcadores$Frecuencia / n_simulaciones
tabla_marcadores$Porcentaje   <- round(tabla_marcadores$Probabilidad * 100, 2)

# Ordenar de mayor a menor probabilidad
tabla_marcadores <- tabla_marcadores[
  order(tabla_marcadores$Probabilidad, decreasing = TRUE), ]
rownames(tabla_marcadores) <- NULL

top15 <- head(tabla_marcadores, 15)


# ============================================================
# BLOQUE 11 — PROBABILIDAD MULTIVARIADA (ANALÍTICA)
# ============================================================
# Propósito: calcular la distribución conjunta de probabilidades
# de forma analítica usando la fórmula de Poisson.
#
# Bajo la hipótesis de independencia entre ambos equipos:
#   P(X = i, Y = j) = P(X = i) × P(Y = j)
#   donde X ~ Poisson(λ_L) y Y ~ Poisson(λ_V)
#
# Se construye una MATRIZ de probabilidades donde:
#   filas    → goles del equipo local (0, 1, 2, ...)
#   columnas → goles del equipo visitante (0, 1, 2, ...)
#
# A partir de esta matriz se calculan todas las probabilidades
# de resultado, over/under y BTTS de forma exacta.

cat("═══════════════════════════════════════════════════════════\n")
cat("  DISTRIBUCIÓN CONJUNTA — PROBABILIDADES ANALÍTICAS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

max_g <- 8  # goles máximos considerados por equipo en la matriz

# Construir matriz de probabilidades conjuntas
prob_matriz <- matrix(0, nrow = max_g + 1, ncol = max_g + 1,
                      dimnames = list(0:max_g, 0:max_g))
for (i in 0:max_g) {
  for (j in 0:max_g) {
    prob_matriz[i + 1, j + 1] <- dpois(i, lambda_local) * dpois(j, lambda_visitante)
  }
}

# Probabilidades de resultado
# Victoria local: la fila i > columna j → triángulo inferior sin diagonal
p_local_win <- 0
p_empate    <- 0
p_visit_win <- 0
for (i in 0:max_g) {
  for (j in 0:max_g) {
    p <- prob_matriz[i + 1, j + 1]
    if (i > j)  p_local_win <- p_local_win + p
    if (i == j) p_empate    <- p_empate    + p
    if (i < j)  p_visit_win <- p_visit_win + p
  }
}

# Over / Under 2.5 goles
p_over25  <- 0
p_under25 <- 0
for (i in 0:max_g) {
  for (j in 0:max_g) {
    p <- prob_matriz[i + 1, j + 1]
    if ((i + j) > 2.5)  p_over25  <- p_over25  + p else
                         p_under25 <- p_under25 + p
  }
}

# BTTS: ambos anotan al menos 1 gol
# P(BTTS) = 1 - P(local no anota) - P(visitante no anota) + P(ninguno anota)
p_btts <- 1 -
          dpois(0, lambda_local) -
          dpois(0, lambda_visitante) +
          dpois(0, lambda_local) * dpois(0, lambda_visitante)

cat("  PROBABILIDADES DE RESULTADO:\n")
cat("  ─────────────────────────────────────────────\n")
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_local,     p_local_win * 100))
cat(sprintf("  Empate                               : %5.1f%%\n", p_empate    * 100))
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_visitante, p_visit_win * 100))
cat("  ─────────────────────────────────────────────\n")
cat(sprintf("  Over  2.5 goles                      : %5.1f%%\n", p_over25  * 100))
cat(sprintf("  Under 2.5 goles                      : %5.1f%%\n", p_under25 * 100))
cat(sprintf("  Ambos equipos anotan (BTTS)          : %5.1f%%\n", p_btts    * 100))
cat("  ─────────────────────────────────────────────\n\n")


# ============================================================
# BLOQUE 12 — ANÁLISIS DE CORRELACIONES
# ============================================================
# Propósito: medir la relación lineal entre las variables
# numéricas del dataset mediante el coeficiente de correlación
# de Pearson (r):
#   r = 1  → relación positiva perfecta
#   r = 0  → sin relación lineal
#   r = -1 → relación negativa perfecta
#
# Correlaciones relevantes a interpretar:
#   • goles_favor  ↔ es_local  → efecto de la localía
#   • goles_favor  ↔ goles_contra → ¿los que anotan también reciben?
#   • goles_favor  ↔ temperatura → efecto del clima
#   • puntos       ↔ goles_favor  → relación rendimiento-resultado

cat("═══════════════════════════════════════════════════════════\n")
cat("  ANÁLISIS DE CORRELACIONES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

vars_cor <- intersect(
  c("goles_favor", "goles_contra", "es_local", "dif_goles", "puntos",
    "temperatura", "humedad", "dias_descanso", "tiros_puerta",
    "posesion", "lesionados", "suspendidos"),
  names(datos))

datos_num <- datos[, vars_cor, drop = FALSE]
datos_num  <- datos_num[complete.cases(datos_num), ]

# Eliminar columnas con varianza cero
vars_var <- vars_cor[sapply(vars_cor, function(v) {
  x <- datos_num[[v]]
  is.numeric(x) && var(x, na.rm = TRUE) > 1e-10
})]
datos_num <- datos_num[, vars_var, drop = FALSE]

mat_cor <- NULL
if (ncol(datos_num) >= 2) {
  mat_cor <- cor(datos_num, use = "complete.obs")

  cat("  Matriz de correlaciones:\n\n")
  print(round(mat_cor, 3))

  if ("goles_favor" %in% rownames(mat_cor)) {
    cat("\n  Correlaciones con 'goles_favor' (ordenadas):\n")
    cor_gf <- sort(mat_cor["goles_favor", ], decreasing = TRUE)
    cor_gf <- cor_gf[names(cor_gf) != "goles_favor"]
    for (nm in names(cor_gf)) {
      barra <- paste(rep(if (cor_gf[nm] > 0) "+" else "-",
                         round(abs(cor_gf[nm]) * 10)), collapse = "")
      cat(sprintf("    %-20s r = %+.3f  %s\n", nm, cor_gf[nm], barra))
    }
  }
} else {
  cat("  Insuficientes variables numéricas para calcular correlaciones.\n")
}
cat("\n")


# ============================================================
# BLOQUE 13 — GRÁFICAS EN R STUDIO
# ============================================================
# Propósito: generar todas las visualizaciones directamente
# en el panel de gráficas de RStudio, sin guardar archivos.
#
# Gráficas incluidas:
#   1.  Goles anotados y recibidos por partido (equipo local)
#   2.  Goles anotados y recibidos por partido (equipo visitante)
#   3.  Forma reciente (barras de puntos por partido)
#   4.  Dispersión: goles anotados vs goles recibidos
#   5.  Histograma: goles simulados equipo local
#   6.  Histograma: goles simulados equipo visitante
#   7.  Barras: top 10 marcadores más probables
#   8.  Mapa de calor: matriz de probabilidades
#   9.  Matriz de correlaciones
#   10. Diagnóstico del modelo de regresión (4 gráficas)
#   11. Comparativa de estadísticas de ambos equipos
#   12. Probabilidades de resultado (barras)

cat("═══════════════════════════════════════════════════════════\n")
cat("  GENERANDO GRÁFICAS...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Paleta de colores corporativos
COL_L  <- "#1B4F72"   # Azul oscuro  → equipo local
COL_V  <- "#922B21"   # Rojo oscuro  → equipo visitante
COL_OK <- "#1ABC9C"   # Verde        → victoria
COL_EM <- "#F39C12"   # Naranja      → empate
COL_DE <- "#E74C3C"   # Rojo claro   → derrota
COL_BG <- "#F8F9FA"   # Gris claro   → fondo

# Función auxiliar: colores de resultado
col_resultado <- function(resultados) {
  ifelse(resultados == "victoria", COL_OK,
  ifelse(resultados == "empate",   COL_EM, COL_DE))
}

# ── GRÁFICA 1 & 2: Goles por partido (línea) ─────────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2), bg = COL_BG)

graficar_goles_partido <- function(df, nombre, col_fav, col_con) {
  n   <- nrow(df)
  idx <- seq_len(n)
  ylim_max <- max(c(df$goles_favor, df$goles_contra), na.rm = TRUE) + 1
  plot(idx, df$goles_favor, type = "b", pch = 19, lwd = 2,
       col = col_fav, ylim = c(0, ylim_max),
       xlab = "Partido (1 = más reciente)", ylab = "Goles",
       main = paste("Evolución de goles —", nombre), xaxt = "n")
  axis(1, at = idx, labels = idx)
  lines(idx, df$goles_contra, type = "b", pch = 17,
        col = col_con, lwd = 2, lty = 2)
  abline(h = mean(df$goles_favor),  col = col_fav, lty = 3, lwd = 1.5)
  abline(h = mean(df$goles_contra), col = col_con, lty = 3, lwd = 1.5)
  legend("topright", c("Goles favor", "Goles contra"),
         col = c(col_fav, col_con), lwd = 2, pch = c(19, 17), lty = c(1, 2),
         bg = "white", cex = 0.85)
  grid(col = "gray90", lty = 1)
}

graficar_goles_partido(datos_local,     equipo_local,     COL_L, COL_V)
graficar_goles_partido(datos_visitante, equipo_visitante, COL_V, COL_L)

# ── GRÁFICA 3: Forma reciente (barras de puntos) ──────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2), bg = COL_BG)

graficar_forma <- function(df, nombre) {
  n       <- nrow(df)
  colores <- col_resultado(df$resultado)
  bp <- barplot(df$puntos, col = colores, border = "white",
                names.arg = paste0("P", seq_len(n)),
                ylim = c(0, 3.8),
                main = paste("Forma reciente —", nombre),
                xlab = "Partido (1 = más reciente)", ylab = "Puntos")
  legend("topright",
         legend = c(sprintf("Victoria (3)  ×%d", sum(df$resultado == "victoria")),
                    sprintf("Empate   (1)  ×%d", sum(df$resultado == "empate")),
                    sprintf("Derrota  (0)  ×%d", sum(df$resultado == "derrota"))),
         fill = c(COL_OK, COL_EM, COL_DE), border = NA, cex = 0.85, bg = "white")
  grid(nx = NA, ny = NULL, col = "gray90")
}

graficar_forma(datos_local,     equipo_local)
graficar_forma(datos_visitante, equipo_visitante)

# ── GRÁFICA 4: Dispersión goles anotados vs recibidos ─────
dev.new(width = 9, height = 7)
par(mar = c(5, 4, 4, 2), bg = COL_BG)

df_disp <- rbind(
  data.frame(nombre = equipo_local,
             gf = datos_local$goles_favor, gc = datos_local$goles_contra,
             col = COL_L, stringsAsFactors = FALSE),
  data.frame(nombre = equipo_visitante,
             gf = datos_visitante$goles_favor, gc = datos_visitante$goles_contra,
             col = COL_V, stringsAsFactors = FALSE))

plot(df_disp$gf, df_disp$gc, col = df_disp$col, pch = 19, cex = 1.6,
     xlab = "Goles anotados", ylab = "Goles recibidos",
     main = "Goles anotados vs. Goles recibidos")
abline(v = mean(df_disp$gf), lty = 3, col = "gray50")
abline(h = mean(df_disp$gc), lty = 3, col = "gray50")
# Líneas de tendencia por equipo
tryCatch({
  df_L  <- df_disp[df_disp$nombre == equipo_local, ]
  df_Vv <- df_disp[df_disp$nombre == equipo_visitante, ]
  if (var(df_L$gf)  > 0) abline(lm(gc ~ gf, df_L),  col = COL_L, lwd = 2, lty = 2)
  if (var(df_Vv$gf) > 0) abline(lm(gc ~ gf, df_Vv), col = COL_V, lwd = 2, lty = 2)
}, error = function(e) invisible(NULL))
legend("topright", c(equipo_local, equipo_visitante),
       col = c(COL_L, COL_V), pch = 19, lty = 2, lwd = 2, bg = "white")
grid(col = "gray90")

# ── GRÁFICA 5 & 6: Histogramas de goles simulados ─────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2), bg = COL_BG)

graficar_hist_sim <- function(goles_sim, lambda, nombre, col_eq) {
  max_g_h <- max(goles_sim)
  breaks  <- seq(-0.5, max_g_h + 0.5, by = 1)
  hist(goles_sim, breaks = breaks, col = col_eq, border = "white",
       freq = FALSE, xlab = "Goles", ylab = "Densidad de probabilidad",
       main = paste("Distribución simulada —", nombre),
       xlim = c(-0.5, min(max_g_h + 0.5, 10)))
  # Curva teórica de Poisson
  goles_seq <- 0:min(max_g_h, 10)
  points(goles_seq, dpois(goles_seq, lambda),
         type = "b", pch = 19, col = "white", lwd = 2)
  abline(v = lambda, col = "yellow", lwd = 2, lty = 2)
  legend("topright",
         legend = c("Simulación", "Teórico Poisson",
                    sprintf("λ = %.3f", lambda)),
         col = c(col_eq, "white", "yellow"),
         pch = c(15, 19, NA), lty = c(NA, 1, 2), lwd = 2,
         bg = "gray20", text.col = "white", cex = 0.85)
}

graficar_hist_sim(goles_L_sim, lambda_local,     equipo_local,     COL_L)
graficar_hist_sim(goles_V_sim, lambda_visitante, equipo_visitante, COL_V)

# ── GRÁFICA 7: Barras — Top 10 marcadores más probables ───
dev.new(width = 11, height = 7)
par(mar = c(5, 5, 4, 2), bg = COL_BG)

top10 <- head(tabla_marcadores, 10)
gradiente <- colorRampPalette(c(COL_L, "#7F8C8D", COL_V))(nrow(top10))

bp7 <- barplot(top10$Porcentaje,
               names.arg = as.character(top10$Marcador),
               col = gradiente, border = "white",
               main = paste("Top 10 marcadores más probables\n",
                            equipo_local, "vs", equipo_visitante),
               xlab = "Marcador", ylab = "Probabilidad (%)",
               ylim = c(0, max(top10$Porcentaje) * 1.25))
text(bp7, top10$Porcentaje + max(top10$Porcentaje) * 0.04,
     paste0(top10$Porcentaje, "%"), cex = 0.9, font = 2)
grid(nx = NA, ny = NULL, col = "gray85")

# ── GRÁFICA 8: Mapa de calor de probabilidades ────────────
max_hm <- min(5, max_g)
hm_data <- as.data.frame(prob_matriz[1:(max_hm + 1), 1:(max_hm + 1)])
hm_df   <- reshape2::melt(as.matrix(hm_data))
names(hm_df) <- c("Goles_Local", "Goles_Visitante", "Probabilidad")
hm_df$Goles_Local     <- as.integer(hm_df$Goles_Local)     - 1L
hm_df$Goles_Visitante <- as.integer(hm_df$Goles_Visitante) - 1L
hm_df$Etiqueta        <- paste0(round(hm_df$Probabilidad * 100, 1), "%")

p_heatmap <- ggplot(hm_df,
    aes(x = factor(Goles_Visitante), y = factor(Goles_Local),
        fill = Probabilidad)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = Etiqueta), color = "white", size = 3.5, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#154360",
    mid      = "#2E86C1",
    high     = "#E74C3C",
    midpoint = median(hm_df$Probabilidad),
    name     = "P(marcador)") +
  scale_x_discrete(name = paste("Goles", equipo_visitante)) +
  scale_y_discrete(name = paste("Goles", equipo_local)) +
  labs(title = paste("Mapa de calor — Probabilidad de marcadores\n",
                     equipo_local, "vs", equipo_visitante)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title     = element_text(hjust = 0.5, face = "bold"),
    axis.text.x    = element_text(angle = 0),
    panel.grid     = element_blank(),
    legend.position = "right")

print(p_heatmap)

# ── GRÁFICA 9: Matriz de correlaciones ────────────────────
if (!is.null(mat_cor) && nrow(mat_cor) >= 2) {
  dev.new(width = 9, height = 8)
  corrplot::corrplot(
    mat_cor,
    method     = "color",
    type       = "upper",
    tl.col     = "black",
    tl.srt     = 45,
    tl.cex     = 0.85,
    addCoef.col = "black",
    number.cex  = 0.70,
    col = colorRampPalette(c(COL_V, "white", COL_L))(200),
    title      = "Matriz de Correlaciones",
    mar        = c(0, 0, 2, 0))
}

# ── GRÁFICA 10: Diagnóstico del modelo de regresión ───────
dev.new(width = 12, height = 10)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
plot(modelo_reg,
     which = c(1, 2, 3, 5),
     sub.caption = paste("Diagnóstico del modelo de regresión —",
                         "Variable dependiente: goles_favor"))

# ── GRÁFICA 11: Comparativa estadísticas de los equipos ───
dev.new(width = 13, height = 6)
par(mar = c(6, 4, 4, 2), bg = COL_BG)

vars_comp  <- c("Goles\nanot.(prom)", "Goles\nrecib.(prom)",
                "Forma\n(pts/15)", "Rend.\nofensivo×3", "Rend.\ndefens.×3")
vals_L_bar <- c(sL$gf_prom, sL$gc_prom, sL$forma_pts,
                sL$rend_ofensivo * 3, sL$rend_defensivo * 3)
vals_V_bar <- c(sV$gf_prom, sV$gc_prom, sV$forma_pts,
                sV$rend_ofensivo * 3, sV$rend_defensivo * 3)

barplot(rbind(vals_L_bar, vals_V_bar), beside = TRUE,
        names.arg = vars_comp, col = c(COL_L, COL_V), border = "white",
        main = paste("Comparativa estadística\n", equipo_local, "vs", equipo_visitante),
        ylab = "Valor", cex.names = 0.85, las = 1)
legend("topright", c(equipo_local, equipo_visitante),
       fill = c(COL_L, COL_V), border = NA, bg = "white")
grid(nx = NA, ny = NULL, col = "gray85")

# ── GRÁFICA 12: Probabilidades de resultado (barras) ──────
dev.new(width = 9, height = 7)
par(mar = c(5, 4, 4, 2), bg = COL_BG)

probs_res <- c(p_local_win, p_empate, p_visit_win) * 100
noms_res  <- c(paste("Victoria\n", equipo_local),
               "Empate",
               paste("Victoria\n", equipo_visitante))
cols_res  <- c(COL_L, COL_EM, COL_V)

bp12 <- barplot(probs_res, names.arg = noms_res, col = cols_res,
                border = "white",
                ylim = c(0, max(probs_res) * 1.30),
                main = paste("Probabilidades de resultado\n",
                             equipo_local, "vs", equipo_visitante),
                ylab = "Probabilidad (%)")
text(bp12, probs_res + max(probs_res) * 0.05,
     paste0(round(probs_res, 1), "%"), cex = 1.2, font = 2)
grid(nx = NA, ny = NULL, col = "gray85")

cat("  ✓ Todas las gráficas generadas. Revise el panel 'Plots' de RStudio.\n\n")


# ============================================================
# BLOQUE 14 — RESUMEN FINAL Y CONCLUSIONES
# ============================================================
# Propósito: mostrar en consola un resumen ejecutivo completo
# con todos los resultados del modelo, interpretación automática
# y advertencia sobre los límites predictivos del modelo.

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN FINAL DE PREDICCIÓN                 ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("  Partido : %s  vs  %s\n", equipo_local, equipo_visitante))
cat(sprintf("  Tipo    : %-15s  Clima: %-10s  %.0f°C / %d%% hum.\n",
            tipo_partido, clima_partido, temperatura_partido, humedad_partido))
cat(sprintf("  Descanso: Local %dd  |  Visitante %dd\n",
            dias_descanso_local, dias_descanso_visitante))
cat("\n")

cat("  ── GOLES ESPERADOS (λ de Poisson) ─────────────────────\n")
cat(sprintf("    %-25s: %.3f goles\n", equipo_local,     lambda_local))
cat(sprintf("    %-25s: %.3f goles\n", equipo_visitante, lambda_visitante))
cat(sprintf("    Total esperado               : %.3f goles\n",
            lambda_local + lambda_visitante))
cat("\n")

cat("  ── MARCADOR MÁS PROBABLE ───────────────────────────────\n")
marcador_1 <- as.character(top15$Marcador[1])
cat(sprintf("    >>> %s  con  %.2f%% de probabilidad <<<\n\n",
            marcador_1, top15$Porcentaje[1]))

cat("  ── TOP 10 MARCADORES MÁS PROBABLES ────────────────────\n")
cat(sprintf("  %-10s  %-12s  %s\n", "Marcador", "Probabilidad", "Barra"))
cat("  ───────────────────────────────────────────────────────\n")
for (i in seq_len(min(10, nrow(tabla_marcadores)))) {
  barra <- paste(rep("█", max(1, round(tabla_marcadores$Porcentaje[i]))),
                 collapse = "")
  cat(sprintf("  %-10s  %6.2f%%       %s\n",
              as.character(tabla_marcadores$Marcador[i]),
              tabla_marcadores$Porcentaje[i],
              barra))
}
cat("  ───────────────────────────────────────────────────────\n\n")

cat("  ── PROBABILIDADES DE RESULTADO ─────────────────────────\n")
cat(sprintf("  %-35s: %5.1f%%\n", paste("Victoria", equipo_local), p_local_win * 100))
cat(sprintf("  %-35s: %5.1f%%\n", "Empate",                        p_empate    * 100))
cat(sprintf("  %-35s: %5.1f%%\n", paste("Victoria", equipo_visitante), p_visit_win * 100))
cat("\n")
cat("  ── MERCADOS ADICIONALES ────────────────────────────────\n")
cat(sprintf("  %-35s: %5.1f%%\n", "Over 2.5 goles",              p_over25 * 100))
cat(sprintf("  %-35s: %5.1f%%\n", "Under 2.5 goles",             p_under25 * 100))
cat(sprintf("  %-35s: %5.1f%%\n", "Ambos equipos anotan (BTTS)", p_btts   * 100))
cat("\n")

# ── Interpretación automática ────────────────────────────
cat("  ── INTERPRETACIÓN AUTOMÁTICA ───────────────────────────\n\n")

# Favorito
if (p_local_win > p_visit_win + 0.10) {
  cat(sprintf("  ▶ FAVORITO: %s (LOCAL) — ventaja de %.1f puntos porcentuales.\n",
              equipo_local, (p_local_win - p_visit_win) * 100))
} else if (p_visit_win > p_local_win + 0.10) {
  cat(sprintf("  ▶ FAVORITO: %s (VISITANTE) — ventaja de %.1f pp.\n",
              equipo_visitante, (p_visit_win - p_local_win) * 100))
} else {
  cat("  ▶ PARTIDO EQUILIBRADO — ninguno con ventaja clara.\n")
}

# Tipo de partido por goles esperados
total_esp <- lambda_local + lambda_visitante
tipo_goles <- if (total_esp > 3.5) "muy abierto, con muchos goles probables (goleada)." else
              if (total_esp > 2.5) "abierto, con varios goles esperados." else
              if (total_esp > 1.5) "equilibrado, con goles escasos." else
              "muy cerrado y defensivo."
cat(sprintf("  ▶ TIPO DE PARTIDO: Se espera un encuentro %s\n", tipo_goles))

# Forma reciente
if (sL$forma_pct > sV$forma_pct + 0.20) {
  cat(sprintf("  ▶ FORMA: %s llega en mejor estado de forma (%d vs %d pts).\n",
              equipo_local, sL$forma_pts, sV$forma_pts))
} else if (sV$forma_pct > sL$forma_pct + 0.20) {
  cat(sprintf("  ▶ FORMA: %s llega en mejor estado de forma (%d vs %d pts).\n",
              equipo_visitante, sV$forma_pts, sL$forma_pts))
} else {
  cat(sprintf("  ▶ FORMA: Ambos equipos llegan en forma similar (%d vs %d pts).\n",
              sL$forma_pts, sV$forma_pts))
}

# Racha
cat(sprintf("  ▶ RACHA LOCAL: %d partido(s) en %s.\n",
            sL$racha_n, sL$racha_tipo))
cat(sprintf("  ▶ RACHA VISITANTE: %d partido(s) en %s.\n",
            sV$racha_n, sV$racha_tipo))

# Clima
if (f_clima < 0.95) {
  cat(sprintf("  ▶ CLIMA: El %s puede reducir el ritmo ofensivo (factor %.2f).\n",
              clima_partido, f_clima))
}

# Fatiga
if (sL$fatiga < 1) {
  cat(sprintf("  ▶ FATIGA: %s tiene solo %d días de descanso (factor %.2f).\n",
              equipo_local, dias_descanso_local, sL$fatiga))
}
if (sV$fatiga < 1) {
  cat(sprintf("  ▶ FATIGA: %s tiene solo %d días de descanso (factor %.2f).\n",
              equipo_visitante, dias_descanso_visitante, sV$fatiga))
}

# Over/Under
if (p_over25 > 0.60) {
  cat("  ▶ GOLES: Alta probabilidad de más de 2.5 goles en el partido.\n")
} else if (p_under25 > 0.60) {
  cat("  ▶ GOLES: Alta probabilidad de menos de 2.5 goles en el partido.\n")
}

# BTTS
if (p_btts > 0.60) {
  cat("  ▶ BTTS: Muy probable que ambos equipos anoten.\n")
} else if (p_btts < 0.40) {
  cat("  ▶ BTTS: Baja probabilidad de que ambos equipos anoten.\n")
}

cat("\n")
cat("  ╔─────────────────────────────────────────────────────╗\n")
cat("  ║  NOTA METODOLÓGICA                                  ║\n")
cat("  ║  Este modelo estima PROBABILIDADES estadísticas     ║\n")
cat("  ║  basadas en datos históricos. No garantiza ni       ║\n")
cat("  ║  predice con certeza el resultado real del partido. ║\n")
cat("  ║  El fútbol contiene varianza aleatoria inherente    ║\n")
cat("  ║  que ningún modelo puede eliminar completamente.    ║\n")
cat("  ╚─────────────────────────────────────────────────────╝\n")

cat("\n  Análisis completado exitosamente.\n")
cat("  Revise las gráficas en el panel 'Plots' de RStudio.\n\n")
