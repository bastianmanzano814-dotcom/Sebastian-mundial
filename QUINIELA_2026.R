# ============================================================
#           QUINIELA 2026
#   Predictor de Marcadores de Fútbol - Mundial 2026
# ============================================================
# Herramienta de análisis estadístico que combina:
#   - Análisis de Regresión Lineal
#   - Procesos Estocásticos (Distribución de Poisson)
#   - Probabilidad Multivariada
#   - Análisis de Correlación
# ============================================================
# AVISO: Este modelo es probabilístico y educativo.
# Los resultados son estimaciones basadas en datos históricos,
# no predicciones con certeza del resultado real.
# ============================================================


# ============================================================
# BLOQUE 1: LIMPIEZA Y CONFIGURACIÓN INICIAL
# ============================================================
# Limpiamos el entorno de R para evitar conflictos con objetos
# de sesiones anteriores y cerramos ventanas gráficas abiertas.

rm(list = ls())      # Elimina todos los objetos del entorno
graphics.off()       # Cierra todas las ventanas gráficas

# ============================================================
# BLOQUE 2: INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================
# Se cargan los paquetes necesarios para el análisis.
# Si no están instalados, se instalan automáticamente.

paquetes_necesarios <- c(
  "readr",       # Leer archivos CSV
  "readxl",      # Leer archivos Excel
  "dplyr",       # Manipulación de datos
  "ggplot2",     # Gráficas modernas
  "corrplot",    # Gráfica de correlación
  "gridExtra",   # Organizar múltiples gráficas
  "scales",      # Escalas en gráficas
  "reshape2",    # Transformar datos
  "tidyr",       # Ordenar datos
  "RColorBrewer",# Paletas de colores
  "viridis"      # Escala de color continua para mapas de calor
)

# Función que instala y carga si aún no está disponible
instalar_y_cargar <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(paquete, character.only = TRUE))
}

invisible(sapply(paquetes_necesarios, instalar_y_cargar))

# Banner de bienvenida
cat("\n")
cat("###############################################################\n")
cat("#                                                             #\n")
cat("#                     QUINIELA 2026                          #\n")
cat("#         Predictor de Marcadores - Fútbol Mundial           #\n")
cat("#                                                             #\n")
cat("#   Regresion + Poisson + Probabilidad Multivariada          #\n")
cat("#                                                             #\n")
cat("###############################################################\n")
cat("\n")


# ============================================================
# BLOQUE 3: PARÁMETROS DEL PARTIDO A PREDECIR
# ============================================================
# *** SOLO MODIFICA ESTA SECCIÓN ***
# Cambia los nombres de los equipos y las condiciones del
# partido que quieres predecir. El resto del código es
# automático.

equipo_1 <- "Argentina"   # <-- CAMBIA el Equipo 1
equipo_2 <- "Francia"     # <-- CAMBIA el Equipo 2

# Condiciones del partido
tipo_partido       <- "Oficial"     # Oficial / Amistoso / Eliminatoria / Copa / Liga de Naciones
dias_descanso_1    <- 7             # Días de descanso del Equipo 1 desde su último partido
dias_descanso_2    <- 7             # Días de descanso del Equipo 2 desde su último partido

# Número de simulaciones para el proceso de Monte Carlo
# A mayor número, más estable la estimación probabilística
n_simulaciones <- 10000

# ============================================================
# BLOQUE 4: CARGA DEL ARCHIVO DE DATOS
# ============================================================
# El programa abre un explorador de archivos para seleccionar
# el dataset histórico. Acepta CSV y Excel (.xlsx / .xls).
# La estructura esperada del archivo se muestra al final de
# este bloque como referencia.

cat("=== PASO 1: CARGANDO DATOS ===\n")
cat("Se abrira un explorador de archivos.\n")
cat("Selecciona el archivo con el historial de partidos...\n\n")

# Para seleccionar el archivo manualmente (recomendado):
ruta_archivo <- file.choose()

# Alternativa: escribe la ruta directa del archivo aquí:
# ruta_archivo <- "C:/ruta/de/tu/archivo/Mundial_Completo.csv"

# Detectar la extensión y cargar con la función adecuada
extension_archivo <- tolower(tools::file_ext(ruta_archivo))

if (extension_archivo == "csv") {
  datos_raw <- read.csv(ruta_archivo, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", na.strings = c("", "NA", "N/A", "-"))
  cat("Archivo CSV cargado correctamente.\n")
} else if (extension_archivo %in% c("xlsx", "xls")) {
  datos_raw <- as.data.frame(readxl::read_excel(ruta_archivo, na = c("", "NA", "N/A", "-")))
  cat("Archivo Excel cargado correctamente.\n")
} else {
  stop("Formato no soportado. Use archivos .csv, .xlsx o .xls")
}

# Referencia de columnas esperadas en el archivo:
# equipo          - Nombre del equipo
# rival           - Nombre del rival
# fecha           - Fecha del partido (YYYY-MM-DD)
# local_visitante - "Local" o "Visitante"
# goles_favor     - Goles anotados por el equipo
# goles_contra    - Goles recibidos por el equipo
# resultado       - "Victoria", "Empate" o "Derrota"
# tipo_partido    - Tipo de competición
# temperatura     - Temperatura en C (numérico)
# humedad         - Humedad en % (numérico)
# dias_descanso   - Días desde el último partido
# tiros_puerta    - Tiros al arco (opcional)
# posesion        - Porcentaje de posesión (opcional)
# lesionados      - Número de lesionados (opcional)
# suspendidos     - Número de suspendidos (opcional)


# ============================================================
# BLOQUE 5: EXPLORACIÓN INICIAL DE LOS DATOS
# ============================================================

cat("\n=== PASO 2: EXPLORACION INICIAL ===\n")
cat("Filas:", nrow(datos_raw), "| Columnas:", ncol(datos_raw), "\n")
cat("Columnas encontradas:", paste(names(datos_raw), collapse = ", "), "\n\n")

# Verificar que las columnas obligatorias existen
columnas_requeridas <- c("equipo", "rival", "goles_favor", "goles_contra",
                         "local_visitante", "resultado")

faltantes <- setdiff(columnas_requeridas, names(datos_raw))
if (length(faltantes) > 0) {
  stop(paste("ERROR: Faltan estas columnas obligatorias:", paste(faltantes, collapse = ", ")))
}

cat("Columnas obligatorias: OK\n")

# Lista de equipos disponibles en el dataset
equipos_disponibles <- sort(unique(datos_raw$equipo))
cat("\nEquipos disponibles en el dataset (", length(equipos_disponibles), "):\n")
cat(paste(equipos_disponibles, collapse = " | "), "\n")


# ============================================================
# BLOQUE 6: LIMPIEZA Y PREPARACIÓN DE DATOS
# ============================================================
# Se estandarizan tipos, se imputan valores faltantes con la
# mediana (método robusto) y se crean variables auxiliares.

cat("\n=== PASO 3: LIMPIEZA DE DATOS ===\n")

datos <- datos_raw

# --- Verificar valores faltantes ---
valores_na <- colSums(is.na(datos))
if (any(valores_na > 0)) {
  cat("Valores faltantes detectados:\n")
  print(valores_na[valores_na > 0])
} else {
  cat("Sin valores faltantes.\n")
}

# --- Convertir fechas ---
if ("fecha" %in% names(datos)) {
  datos$fecha <- as.Date(as.character(datos$fecha))
}

# --- Variables categóricas como factores ---
if ("resultado" %in% names(datos)) {
  datos$resultado <- factor(trimws(datos$resultado),
                            levels = c("Victoria", "Empate", "Derrota"))
}

if ("local_visitante" %in% names(datos)) {
  datos$local_visitante <- factor(trimws(datos$local_visitante),
                                  levels = c("Local", "Visitante"))
}

if ("tipo_partido" %in% names(datos))  datos$tipo_partido <- as.factor(trimws(datos$tipo_partido))
if ("equipo" %in% names(datos))        datos$equipo <- trimws(datos$equipo)
if ("rival" %in% names(datos))         datos$rival  <- trimws(datos$rival)

# --- Variables numéricas: imputar con mediana si hay NA ---
vars_num <- c("goles_favor", "goles_contra", "temperatura", "humedad",
              "dias_descanso", "tiros_puerta", "posesion", "lesionados", "suspendidos")

for (v in vars_num) {
  if (v %in% names(datos)) {
    datos[[v]] <- suppressWarnings(as.numeric(datos[[v]]))
    if (any(is.na(datos[[v]]))) {
      mediana_v <- median(datos[[v]], na.rm = TRUE)
      datos[[v]][is.na(datos[[v]])] <- mediana_v
    }
  }
}

# --- Variables derivadas ---

# Indicador numérico de localía (1 = local, 0 = visitante)
datos$es_local <- ifelse(datos$local_visitante == "Local", 1L, 0L)

# Puntos obtenidos en el partido (escala estándar de fútbol)
datos$puntos_partido <- ifelse(datos$resultado == "Victoria", 3L,
                        ifelse(datos$resultado == "Empate",   1L, 0L))

# Diferencia de goles
datos$diff_goles <- datos$goles_favor - datos$goles_contra

# Indicador binario de victoria
datos$victoria <- as.integer(datos$resultado == "Victoria")

cat("Datos limpios. Filas finales:", nrow(datos), "\n")


# ============================================================
# BLOQUE 7: FUNCIÓN PRINCIPAL - ESTADÍSTICAS POR EQUIPO
# ============================================================
# Esta función extrae y calcula todos los indicadores de un
# equipo usando sus últimos N partidos (por defecto, 50).
# Devuelve una lista con más de 20 métricas que alimentan
# el modelo de regresión y el proceso estocástico.

calcular_estadisticas_equipo <- function(df, nombre_equipo, n = 50) {

  # Filtrar solo los partidos de este equipo
  df_eq <- df[df$equipo == nombre_equipo, ]

  if (nrow(df_eq) == 0) {
    stop(paste("No se encontraron datos para el equipo:", nombre_equipo))
  }

  # Ordenar por fecha si está disponible
  if ("fecha" %in% names(df_eq) && !all(is.na(df_eq$fecha))) {
    df_eq <- df_eq[order(df_eq$fecha), ]
  }

  # Tomar los últimos N partidos
  df_eq <- tail(df_eq, n)
  n_real <- nrow(df_eq)

  # --- Estadísticas básicas ---
  prom_gf <- mean(df_eq$goles_favor,  na.rm = TRUE)   # Promedio goles a favor
  prom_gc <- mean(df_eq$goles_contra, na.rm = TRUE)   # Promedio goles en contra
  prom_dg <- mean(df_eq$diff_goles,   na.rm = TRUE)   # Diferencia media de goles

  victorias <- sum(df_eq$resultado == "Victoria", na.rm = TRUE)
  empates   <- sum(df_eq$resultado == "Empate",   na.rm = TRUE)
  derrotas  <- sum(df_eq$resultado == "Derrota",  na.rm = TRUE)

  # --- Forma reciente: promedio ponderado (partidos más recientes pesan más) ---
  pesos <- seq(from = 0.5, to = 1.5, length.out = n_real)
  forma_ponderada <- sum(df_eq$puntos_partido * pesos, na.rm = TRUE) / sum(pesos)

  # --- Racha de los últimos 5 partidos ---
  ultimos_5 <- tail(df_eq, 5)
  racha_str <- paste(ifelse(ultimos_5$resultado == "Victoria", "G",
                     ifelse(ultimos_5$resultado == "Empate",   "E", "P")),
                     collapse = "-")

  # --- Rendimiento en partidos oficiales ---
  if ("tipo_partido" %in% names(df_eq)) {
    df_ofic <- df_eq[df_eq$tipo_partido %in% c("Oficial", "Eliminatoria", "Copa"), ]
    prom_gf_ofic <- if (nrow(df_ofic) > 0) mean(df_ofic$goles_favor, na.rm = TRUE) else prom_gf
  } else {
    prom_gf_ofic <- prom_gf
  }

  # --- Variables avanzadas opcionales (con valor por defecto si no existen) ---
  prom_tiros     <- if ("tiros_puerta" %in% names(df_eq)) mean(df_eq$tiros_puerta,  na.rm = TRUE) else NA_real_
  prom_posesion  <- if ("posesion"     %in% names(df_eq)) mean(df_eq$posesion,       na.rm = TRUE) else NA_real_
  prom_lesion    <- if ("lesionados"   %in% names(df_eq)) mean(df_eq$lesionados,     na.rm = TRUE) else 0
  prom_suspen    <- if ("suspendidos"  %in% names(df_eq)) mean(df_eq$suspendidos,    na.rm = TRUE) else 0
  prom_descanso  <- if ("dias_descanso" %in% names(df_eq)) mean(df_eq$dias_descanso, na.rm = TRUE) else 7

  # --- Índices de rendimiento ofensivo y defensivo ---
  # Comparados con la media global del dataset
  media_gf_global <- mean(df$goles_favor,  na.rm = TRUE)
  media_gc_global <- mean(df$goles_contra, na.rm = TRUE)

  indice_ofensivo  <- if (media_gf_global > 0) prom_gf / media_gf_global else 1
  indice_defensivo <- if (media_gc_global > 0) 1 - (prom_gc / media_gc_global) else 0

  # Devolver lista completa de estadísticas
  list(
    equipo           = nombre_equipo,
    n_partidos       = n_real,
    prom_gf          = prom_gf,
    prom_gc          = prom_gc,
    prom_dg          = prom_dg,
    victorias        = victorias,
    empates          = empates,
    derrotas         = derrotas,
    forma            = forma_ponderada,
    racha            = racha_str,
    prom_gf_ofic     = prom_gf_ofic,
    indice_ofensivo  = indice_ofensivo,
    indice_defensivo = indice_defensivo,
    prom_tiros       = prom_tiros,
    prom_posesion    = prom_posesion,
    prom_lesion      = prom_lesion,
    prom_suspen      = prom_suspen,
    prom_descanso    = prom_descanso,
    datos            = df_eq
  )
}

# Calcular estadísticas para ambos equipos
cat("\n=== PASO 4: CALCULANDO ESTADISTICAS POR EQUIPO ===\n")

stats_1 <- calcular_estadisticas_equipo(datos, equipo_1)
stats_2 <- calcular_estadisticas_equipo(datos, equipo_2)

cat(sprintf("  Equipo 1 - %s: %.2f goles/partido | Forma: %.2f | Racha: %s\n",
            equipo_1, stats_1$prom_gf, stats_1$forma, stats_1$racha))
cat(sprintf("  Equipo 2 - %s: %.2f goles/partido | Forma: %.2f | Racha: %s\n",
            equipo_2, stats_2$prom_gf, stats_2$forma, stats_2$racha))


# ============================================================
# BLOQUE 8: ENFRENTAMIENTOS DIRECTOS (HEAD-TO-HEAD)
# ============================================================
# Se buscan partidos históricos donde ambos equipos se
# hayan enfrentado entre sí, para incorporar esa tendencia
# en el cálculo de goles esperados.

cat("\n=== PASO 5: ENFRENTAMIENTOS DIRECTOS (H2H) ===\n")

h2h_datos <- datos[
  (datos$equipo == equipo_1 & datos$rival == equipo_2) |
  (datos$equipo == equipo_2 & datos$rival == equipo_1),
]

if (nrow(h2h_datos) > 0) {
  h2h_1 <- h2h_datos[h2h_datos$equipo == equipo_1, ]
  h2h_2 <- h2h_datos[h2h_datos$equipo == equipo_2, ]

  prom_h2h_1 <- if (nrow(h2h_1) > 0) mean(h2h_1$goles_favor, na.rm = TRUE) else stats_1$prom_gf
  prom_h2h_2 <- if (nrow(h2h_2) > 0) mean(h2h_2$goles_favor, na.rm = TRUE) else stats_2$prom_gf

  cat(sprintf("  Encontrados %d enfrentamientos directos.\n", nrow(h2h_datos)))
  cat(sprintf("  Prom. goles %s (Equipo 1) en H2H: %.2f\n", equipo_1, prom_h2h_1))
  cat(sprintf("  Prom. goles %s (Equipo 2) en H2H: %.2f\n", equipo_2, prom_h2h_2))

  # Peso del H2H en la estimación final (20%)
  peso_h2h <- 0.20
} else {
  cat("  Sin enfrentamientos directos disponibles. Se usan solo estadisticas generales.\n")
  prom_h2h_1 <- stats_1$prom_gf
  prom_h2h_2 <- stats_2$prom_gf
  peso_h2h   <- 0
}


# ============================================================
# BLOQUE 10: MODELO DE REGRESIÓN LINEAL
# ============================================================
# Se construye un modelo de regresión sobre el dataset completo.
# La variable dependiente es goles_favor (goles anotados).
# Las variables independientes son todas las numéricas disponibles.
#
# Objetivo del modelo: entender qué variables explican
# la cantidad de goles en un partido y estimar coeficientes
# que se usarán para ajustar las tasas de Poisson.

cat("\n=== PASO 7: MODELO DE REGRESION LINEAL ===\n")

# Seleccionar columnas numéricas disponibles para el modelo
posibles_vars <- c("goles_contra", "es_local", "puntos_partido", "diff_goles",
                   "temperatura", "humedad", "dias_descanso",
                   "tiros_puerta", "posesion", "lesionados", "suspendidos")

vars_disponibles <- intersect(posibles_vars, names(datos))

# Crear data frame solo con columnas numéricas y sin NA
df_reg <- datos[, c("goles_favor", vars_disponibles)]
df_reg <- df_reg[complete.cases(df_reg), ]

# Construir fórmula dinámica
formula_reg <- as.formula(paste("goles_favor ~", paste(vars_disponibles, collapse = " + ")))

# Ajustar el modelo de regresión
modelo_regresion <- lm(formula_reg, data = df_reg)

cat("\n--- Resumen del Modelo de Regresion (Variable: goles_favor) ---\n")
print(summary(modelo_regresion))

# Extraer coeficientes e interpretarlos
cat("\n--- Interpretacion de Coeficientes ---\n")
coef_tabla <- as.data.frame(summary(modelo_regresion)$coefficients)
coef_tabla$significancia <- ifelse(coef_tabla[, 4] < 0.001, "***",
                            ifelse(coef_tabla[, 4] < 0.01,  "**",
                            ifelse(coef_tabla[, 4] < 0.05,  "*",
                            ifelse(coef_tabla[, 4] < 0.10,  ".", " "))))
cols_num <- sapply(coef_tabla, is.numeric)
coef_tabla[, cols_num] <- round(coef_tabla[, cols_num], 4)
print(coef_tabla)

cat("\n  *** p<0.001  ** p<0.01  * p<0.05  . p<0.10\n")
cat("  R2 ajustado:", round(summary(modelo_regresion)$adj.r.squared, 4), "\n")
cat("  F-estadistico:", round(summary(modelo_regresion)$fstatistic[1], 3), "\n")

# Valores ajustados y residuos
datos_reg_ajuste <- df_reg
datos_reg_ajuste$fitted    <- fitted(modelo_regresion)
datos_reg_ajuste$residuos  <- residuals(modelo_regresion)


# ============================================================
# BLOQUE 11: CÁLCULO DE LAMBDA (TASAS DE POISSON)
# ============================================================
# lambda representa la tasa media de goles esperados para
# cada equipo. Es el parámetro clave del proceso de Poisson.
# Se combina: estadísticas recientes + H2H
# + ventaja de localía + penalización por fatiga/lesiones.

cat("\n=== PASO 8: CALCULANDO LAMBDAS (TASAS DE ANOTACION) ===\n")
cat("  Ambos equipos en cancha neutral — sin ventaja de localía.\n")

media_gc_global <- mean(datos$goles_contra, na.rm = TRUE)

# --- Lambda Equipo 1 ---
# Ambos equipos usan prom_gf general (sin distinción local/visitante)
base_1 <- stats_1$prom_gf * 0.80 +
          prom_h2h_1      * peso_h2h +
          stats_1$prom_gf * (0.20 - peso_h2h)

# Factor defensivo del rival (cuánto deja pasar)
factor_def_2 <- if (media_gc_global > 0) stats_2$prom_gc / media_gc_global else 1

lambda_1 <- base_1 * factor_def_2

if (stats_1$forma > 1.8) lambda_1 <- lambda_1 * 1.06
if (stats_1$forma < 0.8) lambda_1 <- lambda_1 * 0.92

if (dias_descanso_1 < 4)  lambda_1 <- lambda_1 * 0.90
if (dias_descanso_1 > 10) lambda_1 <- lambda_1 * 1.04

penalizacion_1 <- 1 - (stats_1$prom_lesion * 0.025 + stats_1$prom_suspen * 0.035)
lambda_1 <- lambda_1 * max(penalizacion_1, 0.70)

lambda_1 <- max(0.30, min(lambda_1, 5.0))

# --- Lambda Equipo 2 ---
base_2 <- stats_2$prom_gf * 0.80 +
          prom_h2h_2      * peso_h2h +
          stats_2$prom_gf * (0.20 - peso_h2h)

factor_def_1 <- if (media_gc_global > 0) stats_1$prom_gc / media_gc_global else 1

lambda_2 <- base_2 * factor_def_1

if (stats_2$forma > 1.8) lambda_2 <- lambda_2 * 1.06
if (stats_2$forma < 0.8) lambda_2 <- lambda_2 * 0.92

if (dias_descanso_2 < 4)  lambda_2 <- lambda_2 * 0.90
if (dias_descanso_2 > 10) lambda_2 <- lambda_2 * 1.04

penalizacion_2 <- 1 - (stats_2$prom_lesion * 0.025 + stats_2$prom_suspen * 0.035)
lambda_2 <- lambda_2 * max(penalizacion_2, 0.70)

lambda_2 <- max(0.30, min(lambda_2, 5.0))

cat(sprintf("  Lambda Equipo 1 (%s) = %.4f goles esperados\n", equipo_1, lambda_1))
cat(sprintf("  Lambda Equipo 2 (%s) = %.4f goles esperados\n", equipo_2, lambda_2))


# ============================================================
# BLOQUE 12: SIMULACIÓN MONTE CARLO (PROCESO ESTOCÁSTICO)
# ============================================================
# Se simulan n_simulaciones partidos completos.
# Cada partido se genera con dos variables de Poisson
# independientes (goles Equipo 1 y Equipo 2).
# La frecuencia relativa de cada marcador se convierte en
# probabilidad estimada.

cat("\n=== PASO 9: SIMULACION MONTE CARLO ===\n")
cat(sprintf("  Simulando %d partidos...\n", n_simulaciones))

set.seed(2026)

# Proceso de Poisson: genera conteos de eventos (goles)
# que ocurren con tasa constante lambda en un intervalo fijo
goles_1_sim <- rpois(n_simulaciones, lambda_1)
goles_2_sim <- rpois(n_simulaciones, lambda_2)

# Crear etiqueta de marcador para cada simulación
marcadores_sim <- paste(goles_1_sim, goles_2_sim, sep = "-")

# Tabla de frecuencias convertida a probabilidades
tabla_freq <- as.data.frame(table(marcadores_sim), stringsAsFactors = FALSE)
names(tabla_freq) <- c("Marcador", "Frecuencia")
tabla_freq$Probabilidad     <- tabla_freq$Frecuencia / n_simulaciones
tabla_freq$Probabilidad_pct <- paste0(round(tabla_freq$Probabilidad * 100, 2), "%")

# Ordenar por probabilidad descendente
tabla_freq <- tabla_freq[order(-tabla_freq$Probabilidad), ]
rownames(tabla_freq) <- NULL

cat(sprintf("  Marcadores distintos generados: %d\n", nrow(tabla_freq)))
cat("  Simulacion completada.\n")


# ============================================================
# BLOQUE 13: PROBABILIDAD MULTIVARIADA
# ============================================================
# La distribución conjunta de (Goles_L, Goles_V) se modela
# como el producto de dos Poisson independientes. Esto permite
# calcular exactamente la probabilidad de cada marcador
# sin depender de la simulación.

cat("\n=== PASO 10: PROBABILIDAD MULTIVARIADA ===\n")

# Matriz de probabilidades exactas para marcadores 0-7 vs 0-7
max_g <- 7
mat_prob <- matrix(0, nrow = max_g + 1, ncol = max_g + 1,
                   dimnames = list(paste0(0:max_g), paste0(0:max_g)))

for (i in 0:max_g) {
  for (j in 0:max_g) {
    mat_prob[i + 1, j + 1] <- dpois(i, lambda_1) * dpois(j, lambda_2)
  }
}

# Probabilidades de resultado final
prob_gana_1   <- sum(goles_1_sim > goles_2_sim)  / n_simulaciones
prob_empate   <- sum(goles_1_sim == goles_2_sim) / n_simulaciones
prob_gana_2   <- sum(goles_1_sim < goles_2_sim)  / n_simulaciones

prob_over_2p5  <- sum((goles_1_sim + goles_2_sim) > 2.5)  / n_simulaciones
prob_under_2p5 <- sum((goles_1_sim + goles_2_sim) <= 2.5) / n_simulaciones
prob_bts       <- sum(goles_1_sim > 0 & goles_2_sim > 0)  / n_simulaciones

cat(sprintf("  Gana Equipo 1 %-20s : %5.1f%%\n", equipo_1, prob_gana_1 * 100))
cat(sprintf("  Empate                           : %5.1f%%\n", prob_empate  * 100))
cat(sprintf("  Gana Equipo 2 %-20s : %5.1f%%\n", equipo_2, prob_gana_2 * 100))
cat(sprintf("  Over  2.5 goles                  : %5.1f%%\n", prob_over_2p5  * 100))
cat(sprintf("  Under 2.5 goles                  : %5.1f%%\n", prob_under_2p5 * 100))
cat(sprintf("  Ambos equipos anotan (BTTS)      : %5.1f%%\n", prob_bts * 100))


# ============================================================
# BLOQUE 14: MATRIZ DE CORRELACIÓN
# ============================================================
# Análisis de correlaciones de Pearson entre todas las
# variables numéricas disponibles en el dataset completo.

cat("\n=== PASO 11: MATRIZ DE CORRELACION ===\n")

cols_cor <- intersect(c("goles_favor", "goles_contra", "es_local",
                         "puntos_partido", "diff_goles", "temperatura",
                         "humedad", "dias_descanso", "tiros_puerta",
                         "posesion", "lesionados", "suspendidos"),
                      names(datos))

df_cor <- datos[, cols_cor]
df_cor <- df_cor[, sapply(df_cor, is.numeric)]
df_cor <- na.omit(df_cor)

mat_cor <- cor(df_cor, use = "complete.obs")

cat("\nCorrelaciones destacadas:\n")
cat(sprintf("  goles_favor  vs goles_contra : %+.3f\n", mat_cor["goles_favor", "goles_contra"]))
if ("es_local" %in% colnames(mat_cor))
  cat(sprintf("  goles_favor  vs localía       : %+.3f\n", mat_cor["goles_favor", "es_local"]))
if ("temperatura" %in% colnames(mat_cor))
  cat(sprintf("  goles_favor  vs temperatura   : %+.3f\n", mat_cor["goles_favor", "temperatura"]))
if ("dias_descanso" %in% colnames(mat_cor))
  cat(sprintf("  goles_favor  vs descanso      : %+.3f\n", mat_cor["goles_favor", "dias_descanso"]))
if ("tiros_puerta" %in% colnames(mat_cor) && "goles_favor" %in% colnames(mat_cor))
  cat(sprintf("  goles_favor  vs tiros_puerta  : %+.3f\n", mat_cor["goles_favor", "tiros_puerta"]))


# ============================================================
# BLOQUE 15: GENERACIÓN DE GRÁFICAS EN R STUDIO
# ============================================================
# Se generan las 10 gráficas requeridas. Todas se abren
# directamente en el panel de gráficas de R Studio.
# No se guardan archivos externos.

cat("\n=== PASO 12: GENERANDO GRAFICAS EN R STUDIO ===\n")

# Paleta de colores
col_1 <- "#1565C0"   # Azul oscuro = Equipo 1
col_2 <- "#B71C1C"   # Rojo oscuro = Equipo 2

# Últimos 50 partidos de cada equipo
df_E50_1 <- stats_1$datos
df_E50_2 <- stats_2$datos
df_E50_1$num <- seq_len(nrow(df_E50_1))
df_E50_2$num <- seq_len(nrow(df_E50_2))
df_E50_1$equipo_label <- paste0("Equipo 1: ", equipo_1)
df_E50_2$equipo_label <- paste0("Equipo 2: ", equipo_2)

df_ambos <- rbind(df_E50_1, df_E50_2)
etiquetas_eq <- c(paste0("Equipo 1: ", equipo_1), paste0("Equipo 2: ", equipo_2))


# --- GRAFICA 1: Dispersion goles favor vs goles contra ---
cat("  Generando grafica 1: Dispersion goles...\n")

g1 <- ggplot(df_ambos, aes(x = goles_favor, y = goles_contra, color = equipo_label)) +
  geom_jitter(size = 4, alpha = 0.75, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_color_manual(values = setNames(c(col_1, col_2), etiquetas_eq)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = "Línea punteada = equilibrio", size = 3, color = "gray40") +
  labs(title    = "Goles Anotados vs Goles Recibidos",
       subtitle = paste("Últimos 50 partidos |", equipo_1, "vs", equipo_2),
       x = "Goles Anotados", y = "Goles Recibidos", color = "Equipo") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "top")

print(g1)
Sys.sleep(0.3)


# --- GRAFICA 2: Forma reciente (puntos por partido) ---
cat("  Generando grafica 2: Forma reciente...\n")

g2 <- ggplot(df_ambos, aes(x = num, y = puntos_partido,
                            color = equipo_label, group = equipo_label)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_manual(values = setNames(c(col_1, col_2), etiquetas_eq)) +
  scale_y_continuous(breaks = c(0, 1, 3),
                     labels = c("Derrota (0)", "Empate (1)", "Victoria (3)"),
                     limits = c(-0.2, 3.3)) +
  labs(title    = "Forma Reciente — Últimos 50 Partidos",
       subtitle = "Puntos obtenidos por fecha (cronológico de izq. a der.)",
       x = "Partido (de más antiguo a más reciente)",
       y = "Puntos obtenidos",
       color = "Equipo") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        legend.position = "top")

print(g2)
Sys.sleep(0.3)


# --- GRAFICA 3: Barras de goles anotados y recibidos ---
cat("  Generando grafica 3: Goles por partido...\n")

df_long <- tidyr::pivot_longer(df_ambos,
                                cols      = c("goles_favor", "goles_contra"),
                                names_to  = "tipo",
                                values_to = "goles")
df_long$tipo <- ifelse(df_long$tipo == "goles_favor", "Anotados", "Recibidos")

g3 <- ggplot(df_long, aes(x = num, y = goles, fill = tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ equipo_label, ncol = 1) +
  scale_fill_manual(values = c("Anotados" = "#1976D2", "Recibidos" = "#D32F2F")) +
  labs(title    = "Goles Anotados y Recibidos por Partido (Últimos 50)",
       x = "Número de partido (cronológico)",
       y = "Goles",
       fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "top")

print(g3)
Sys.sleep(0.3)


# --- GRAFICA 4: Matriz de correlacion ---
cat("  Generando grafica 4: Matriz de correlacion...\n")

dev.new(width = 9, height = 8)
corrplot::corrplot(
  mat_cor,
  method       = "color",
  type         = "upper",
  addCoef.col  = "black",
  tl.col       = "black",
  tl.srt       = 45,
  tl.cex       = 0.85,
  number.cex   = 0.70,
  col          = colorRampPalette(c("#B71C1C", "white", "#1565C0"))(200),
  title        = "Matriz de Correlación entre Variables del Partido",
  mar          = c(0, 0, 2, 0)
)
Sys.sleep(0.3)


# --- GRAFICAS 5 Y 6: Diagnostico del modelo de regresion ---
cat("  Generando graficas 5-6: Diagnostico de regresion...\n")

dev.new(width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(modelo_regresion,
     main = "Diagnóstico del Modelo de Regresión Lineal",
     col  = "steelblue", pch = 16, cex = 0.6)
par(mfrow = c(1, 1))
Sys.sleep(0.3)


# --- GRAFICA 7: Histograma goles simulados - Equipo 1 ---
cat("  Generando grafica 7: Histograma goles", equipo_1, "...\n")

df_hist_1 <- data.frame(goles = goles_1_sim)

g7 <- ggplot(df_hist_1, aes(x = goles)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, fill = col_1, color = "white", alpha = 0.85) +
  stat_function(fun = dpois, args = list(lambda = lambda_1),
                geom = "point", color = "orange", size = 3.5, n = 9) +
  geom_vline(xintercept = lambda_1, color = "orange",
             linetype = "dashed", linewidth = 1.4) +
  annotate("text", x = lambda_1 + 0.4, y = Inf,
           vjust = 2, label = paste0("λ = ", round(lambda_1, 3)),
           color = "darkorange", fontface = "bold", size = 4.5) +
  scale_x_continuous(breaks = 0:max(goles_1_sim)) +
  labs(title    = paste("Distribución de Poisson — Goles Equipo 1:", equipo_1),
       subtitle = paste0("λ = ", round(lambda_1, 4),
                         " | Media simulada: ", round(mean(goles_1_sim), 4)),
       x = "Goles en el partido", y = "Densidad de probabilidad") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, color = col_1),
        plot.subtitle = element_text(color = "gray40"))

print(g7)
Sys.sleep(0.3)


# --- GRAFICA 8: Histograma goles simulados - Equipo 2 ---
cat("  Generando grafica 8: Histograma goles", equipo_2, "...\n")

df_hist_2 <- data.frame(goles = goles_2_sim)

g8 <- ggplot(df_hist_2, aes(x = goles)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, fill = col_2, color = "white", alpha = 0.85) +
  stat_function(fun = dpois, args = list(lambda = lambda_2),
                geom = "point", color = "gold", size = 3.5, n = 9) +
  geom_vline(xintercept = lambda_2, color = "gold",
             linetype = "dashed", linewidth = 1.4) +
  annotate("text", x = lambda_2 + 0.4, y = Inf,
           vjust = 2, label = paste0("λ = ", round(lambda_2, 3)),
           color = "goldenrod3", fontface = "bold", size = 4.5) +
  scale_x_continuous(breaks = 0:max(goles_2_sim)) +
  labs(title    = paste("Distribución de Poisson — Goles Equipo 2:", equipo_2),
       subtitle = paste0("λ = ", round(lambda_2, 4),
                         " | Media simulada: ", round(mean(goles_2_sim), 4)),
       x = "Goles en el partido", y = "Densidad de probabilidad") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, color = col_2),
        plot.subtitle = element_text(color = "gray40"))

print(g8)
Sys.sleep(0.3)


# --- GRAFICA 9: Barras de los marcadores mas probables ---
cat("  Generando grafica 9: Top marcadores...\n")

top10 <- head(tabla_freq, 10)
top10$Marcador <- factor(top10$Marcador, levels = rev(top10$Marcador))

g9 <- ggplot(top10, aes(x = Marcador, y = Probabilidad * 100, fill = Probabilidad)) +
  geom_col(color = "white", width = 0.7) +
  geom_text(aes(label = Probabilidad_pct), hjust = -0.12,
            fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = -1, guide = "none") +
  labs(title    = "Top 10 Marcadores Más Probables",
       subtitle = paste("Equipo 1:", equipo_1, " | Equipo 2:", equipo_2),
       x = "Marcador", y = "Probabilidad (%)") +
  expand_limits(y = max(top10$Probabilidad * 100) * 1.20) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.y   = element_text(size = 12, face = "bold"))

print(g9)
Sys.sleep(0.3)


# --- GRAFICA 10: Mapa de calor de probabilidades de marcadores ---
cat("  Generando grafica 10: Mapa de calor...\n")

max_g_calor <- 5
calor_df <- expand.grid(G1 = 0:max_g_calor, G2 = 0:max_g_calor)
calor_df$Probabilidad <- mapply(
  function(i, j) dpois(i, lambda_1) * dpois(j, lambda_2),
  calor_df$G1, calor_df$G2
)
calor_df$Etiqueta <- paste0(round(calor_df$Probabilidad * 100, 1), "%")

g10 <- ggplot(calor_df, aes(x = G2, y = G1, fill = Probabilidad)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = Etiqueta), size = 3.2, fontface = "bold") +
  scale_fill_gradient2(
    low      = "white",
    mid      = "#FFF9C4",
    high     = "#B71C1C",
    midpoint = quantile(calor_df$Probabilidad, 0.70),
    labels   = scales::percent_format(accuracy = 0.1),
    name     = "Prob."
  ) +
  scale_x_continuous(breaks = 0:max_g_calor,
                     name   = paste("Goles Equipo 2:", equipo_2)) +
  scale_y_continuous(breaks = 0:max_g_calor,
                     name   = paste("Goles Equipo 1:", equipo_1)) +
  labs(title    = "Mapa de Calor — Probabilidades de Marcadores",
       subtitle = paste0(equipo_1, " (filas) vs ", equipo_2, " (columnas)")) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        axis.text     = element_text(size = 11, face = "bold"),
        panel.grid    = element_blank())

print(g10)

cat("\n  Todas las graficas han sido generadas en R Studio.\n")


# ============================================================
# BLOQUE 16: TABLA FINAL DE MARCADORES
# ============================================================

cat("\n")
cat("################################################################\n")
cat("#            QUINIELA 2026 — TABLA DE RESULTADOS              #\n")
cat(sprintf("#  Eq.1: %-17s  vs  Eq.2: %-17s  #\n",
            substr(equipo_1, 1, 17), substr(equipo_2, 1, 17)))
cat("################################################################\n\n")

top_tabla <- head(tabla_freq, 15)
top_tabla$Posicion <- seq_len(nrow(top_tabla))
top_tabla <- top_tabla[, c("Posicion", "Marcador", "Probabilidad_pct")]
names(top_tabla) <- c("Posicion", "Marcador", "Probabilidad")

print(top_tabla, row.names = FALSE)

marcador_top <- as.character(tabla_freq$Marcador[1])
prob_top_str <- tabla_freq$Probabilidad_pct[1]
prob_top_num <- tabla_freq$Probabilidad[1] * 100


# ============================================================
# BLOQUE 17: PREDICCIÓN FINAL E INTERPRETACIÓN AUTOMÁTICA
# ============================================================

cat("\n")
cat("################################################################\n")
cat("#                  PREDICCION FINAL                           #\n")
cat("################################################################\n")

cat(sprintf("\n  MARCADOR MAS PROBABLE:  %s\n", marcador_top))
cat(sprintf("  Probabilidad estimada:  %s (%.1f%%)\n", prob_top_str, prob_top_num))

cat(sprintf("\n  Lambda Equipo 1 %-17s : %.4f goles esperados\n", equipo_1, lambda_1))
cat(sprintf("  Lambda Equipo 2 %-17s : %.4f goles esperados\n", equipo_2, lambda_2))

cat("\n--- PROBABILIDADES DE RESULTADO ---\n")
cat(sprintf("  Gana Equipo 1 %-20s : %5.1f%%\n", equipo_1, prob_gana_1 * 100))
cat(sprintf("  Empate                           : %5.1f%%\n", prob_empate  * 100))
cat(sprintf("  Gana Equipo 2 %-20s : %5.1f%%\n", equipo_2, prob_gana_2 * 100))

cat("\n--- MERCADOS ADICIONALES ---\n")
cat(sprintf("  Over  2.5 goles                  : %5.1f%%\n", prob_over_2p5  * 100))
cat(sprintf("  Under 2.5 goles                  : %5.1f%%\n", prob_under_2p5 * 100))
cat(sprintf("  Ambos equipos anotan (BTTS)      : %5.1f%%\n", prob_bts * 100))

cat("\n--- ESTADISTICAS DE CADA EQUIPO (ultimos 50 partidos) ---\n")
cat(sprintf("\n  Variable               Equipo 1: %-12s  Equipo 2: %-12s\n", equipo_1, equipo_2))
cat(paste0(rep("-", 65), collapse = ""), "\n")
cat(sprintf("  Prom. goles favor    :  %-18.2f  %.2f\n", stats_1$prom_gf,    stats_2$prom_gf))
cat(sprintf("  Prom. goles contra   :  %-18.2f  %.2f\n", stats_1$prom_gc,    stats_2$prom_gc))
cat(sprintf("  Victorias (50P)      :  %-18d  %d\n",     stats_1$victorias,  stats_2$victorias))
cat(sprintf("  Empates   (50P)      :  %-18d  %d\n",     stats_1$empates,    stats_2$empates))
cat(sprintf("  Derrotas  (50P)      :  %-18d  %d\n",     stats_1$derrotas,   stats_2$derrotas))
cat(sprintf("  Forma ponderada      :  %-18.3f  %.3f\n", stats_1$forma,      stats_2$forma))
cat(sprintf("  Racha (ultimos 5P)   :  %-18s  %s\n",     stats_1$racha,      stats_2$racha))
cat(sprintf("  Indice ofensivo      :  %-18.3f  %.3f\n", stats_1$indice_ofensivo, stats_2$indice_ofensivo))
cat(sprintf("  Indice defensivo     :  %-18.3f  %.3f\n", stats_1$indice_defensivo, stats_2$indice_defensivo))

cat("\n--- CONDICIONES DEL PARTIDO ---\n")
cat(sprintf("  Tipo de partido  : %s\n",   tipo_partido))
cat(sprintf("  Descanso Equipo 1: %d dias\n", dias_descanso_1))
cat(sprintf("  Descanso Equipo 2: %d dias\n", dias_descanso_2))

# --- Interpretación automática ---
cat("\n--- INTERPRETACION DEL MODELO ---\n")

# Quién es favorito
diferencia_prob <- prob_gana_1 - prob_gana_2
if (diferencia_prob > 0.15) {
  interpretacion_fav <- paste0("Equipo 1 (", equipo_1, ") es el CLARO FAVORITO del partido.")
} else if (diferencia_prob < -0.15) {
  interpretacion_fav <- paste0("Equipo 2 (", equipo_2, ") es el CLARO FAVORITO del partido.")
} else if (abs(diferencia_prob) < 0.05) {
  interpretacion_fav <- "El partido está MUY EQUILIBRADO. No hay favorito claro."
} else {
  fav <- if (diferencia_prob > 0) paste0("Equipo 1 (", equipo_1, ")") else paste0("Equipo 2 (", equipo_2, ")")
  interpretacion_fav <- paste0(fav, " tiene una LIGERA VENTAJA según el modelo.")
}

# Over/Under
interpretacion_goles <- ifelse(prob_over_2p5 > 0.55,
  "Se espera un PARTIDO CON MUCHOS GOLES (Over 2.5 es mas probable).",
  "Se espera un PARTIDO CON POCOS GOLES (Under 2.5 es mas probable).")

# BTTS
interpretacion_btts <- ifelse(prob_bts > 0.55,
  "Es probable que AMBOS EQUIPOS ANOTEN al menos un gol.",
  "Es posible que UNO DE LOS EQUIPOS NO LOGRE ANOTAR.")

# Forma reciente comparada
if (stats_1$forma > stats_2$forma + 0.5) {
  interpretacion_forma <- paste0("Equipo 1 (", equipo_1, ") llega con MEJOR FORMA RECIENTE.")
} else if (stats_2$forma > stats_1$forma + 0.5) {
  interpretacion_forma <- paste0("Equipo 2 (", equipo_2, ") llega con MEJOR FORMA RECIENTE.")
} else {
  interpretacion_forma <- "Ambos equipos llegan con FORMAS SIMILARES."
}

cat(sprintf("\n  1. %s\n", interpretacion_fav))
cat(sprintf("  2. %s\n", interpretacion_goles))
cat(sprintf("  3. %s\n", interpretacion_btts))
cat(sprintf("  4. %s\n", interpretacion_forma))
cat(sprintf("  5. El marcador mas probable segun %d simulaciones de Poisson es: %s\n",
            n_simulaciones, marcador_top))
cat(sprintf("     con una probabilidad estimada de %.1f%%.\n", prob_top_num))

if (nrow(h2h_datos) > 0) {
  cat(sprintf("  6. Los %d enfrentamientos directos entre ambos equipos\n", nrow(h2h_datos)))
  cat(sprintf("     muestran un promedio de %.2f goles para Equipo 1 (%s)\n", prom_h2h_1, equipo_1))
  cat(sprintf("     y %.2f goles para Equipo 2 (%s).\n", prom_h2h_2, equipo_2))
} else {
  cat("  6. No se hallaron enfrentamientos directos previos en el dataset.\n")
}

cat("\n")
cat("################################################################\n")
cat("#  AVISO IMPORTANTE:                                          #\n")
cat("#  Este modelo es de naturaleza probabilistica y educativa.   #\n")
cat("#  Los resultados son estimaciones basadas en datos           #\n")
cat("#  historicos, no certezas ni garantias del resultado real.   #\n")
cat("#  El futbol siempre puede sorprender.                        #\n")
cat("#                                                             #\n")
cat("#            QUINIELA 2026 - Solo fines educativos            #\n")
cat("################################################################\n\n")
