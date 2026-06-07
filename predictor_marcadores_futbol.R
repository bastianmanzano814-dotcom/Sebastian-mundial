# ============================================================
#  PREDICTOR DE MARCADORES DE FÚTBOL
#  Modelo: Regresión Lineal + Proceso de Poisson +
#          Probabilidad Multivariada + Correlaciones
# ============================================================
#
# FORMATO DEL ARCHIVO DE DATOS (CSV o XLSX):
#
#  OBLIGATORIAS:
#   equipo          → Nombre del equipo (o 1ª columna si no se llama así)
#   rival           → Nombre del rival
#   local_visitante → "Local" o "Visitante"
#   goles_favor     → Goles anotados
#   goles_contra    → Goles recibidos
#   resultado       → "Victoria", "Empate" o "Derrota"
#
#  RECOMENDADAS:
#   fecha           → Fecha del partido (YYYY-MM-DD)
#   tipo_partido    → "Liga","Copa","Eliminatoria","Oficial","Amistoso", etc.
#   clima           → "Soleado","Nublado","Lluvia","Frio","Caluroso"
#   temperatura     → Grados Celsius
#   humedad         → Porcentaje 0-100
#   dias_descanso   → Días desde el último partido
#
#  OPCIONALES:
#   tiros_puerta, posesion, lesionados, suspendidos
#
# ── INSTRUCCIONES DE USO ────────────────────────────────────
#  1. Seleccione Ctrl+A y luego Ctrl+Enter para correr todo.
#  2. Se abrirá el explorador para elegir el archivo de datos.
#  3. El programa le mostrará los equipos disponibles y le
#     pedirá que ingrese los nombres en la consola.
#  4. Ingrese los demás parámetros del partido cuando se soliciten.
# ============================================================


# ============================================================
# BLOQUE 1 — INSTALACIÓN Y CARGA DE LIBRERÍAS
# ============================================================

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
# BLOQUE 2 — PARÁMETROS FIJOS (edite solo estos números)
# ============================================================
# Los equipos y condiciones del partido se pedirán
# automáticamente después de cargar el archivo.

n_simulaciones        <- 10000   # Simulaciones Monte Carlo (recomendado: 10000)
max_partidos_analizar <- 10      # Últimos N partidos a analizar por equipo


# ============================================================
# BLOQUE 3 — CARGA DE DATOS
# ============================================================
# Se abre el explorador de archivos para seleccionar el CSV o XLSX.

cat(">>> Seleccione el archivo de datos históricos...\n")
cat("    (Formatos aceptados: .csv  |  .xlsx  |  .xls)\n\n")

archivo   <- file.choose()
extension <- tolower(tools::file_ext(archivo))

if (extension == "csv") {
  datos <- tryCatch(
    read.csv(archivo, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e)
      read.csv2(archivo, stringsAsFactors = FALSE, encoding = "UTF-8")
  )
  cat("  Archivo CSV cargado correctamente.\n")
} else if (extension %in% c("xlsx", "xls")) {
  datos <- as.data.frame(readxl::read_excel(archivo))
  cat("  Archivo Excel cargado correctamente.\n")
} else {
  stop("Formato no soportado. Use .csv o .xlsx")
}

cat(sprintf("  Filas cargadas : %d\n", nrow(datos)))
cat(sprintf("  Columnas       : %d\n\n", ncol(datos)))


# ============================================================
# BLOQUE 4 — LIMPIEZA Y PREPARACIÓN DE DATOS
# ============================================================

# ── 4.1  Estandarizar nombres de columnas ────────────────
names(datos) <- tolower(trimws(gsub("[^a-zA-Z0-9_]", "_", names(datos))))

cat("  Columnas detectadas en el archivo:\n")
cat(paste0("    ", paste(names(datos), collapse = ", ")), "\n\n")

# ── 4.2  Detectar columna de nombres de equipos ──────────
# Se acepta la columna "equipo" o, si no existe, se usa la 1ª columna.
if ("equipo" %in% names(datos)) {
  col_equipo <- "equipo"
  cat("  Columna de equipos detectada: 'equipo'\n\n")
} else {
  col_equipo <- names(datos)[1]
  cat(sprintf("  Columna 'equipo' no encontrada. Usando primera columna: '%s'\n\n",
              col_equipo))
  # Renombrar para unificar el resto del código
  names(datos)[names(datos) == col_equipo] <- "equipo"
}

# ── 4.3  Verificar columnas obligatorias ─────────────────
cols_obligatorias <- c("equipo", "goles_favor", "goles_contra",
                       "local_visitante", "resultado")

# Intentar mapear columnas con nombres alternativos comunes
alias <- list(
  goles_favor   = c("goles_a_favor", "goles_anotados", "gf", "favor"),
  goles_contra  = c("goles_en_contra", "goles_recibidos", "gc", "contra"),
  local_visitante = c("condicion", "localidad", "local"),
  resultado     = c("result", "outcome")
)
for (col_std in names(alias)) {
  if (!col_std %in% names(datos)) {
    for (alt in alias[[col_std]]) {
      if (alt %in% names(datos)) {
        names(datos)[names(datos) == alt] <- col_std
        cat(sprintf("  AVISO: Columna '%s' mapeada desde '%s'.\n", col_std, alt))
        break
      }
    }
  }
}

cols_faltantes <- cols_obligatorias[!cols_obligatorias %in% names(datos)]
if (length(cols_faltantes) > 0) {
  stop(paste0(
    "\nERROR: Columnas obligatorias no encontradas:\n  ",
    paste(cols_faltantes, collapse = ", "),
    "\n\nColumnas disponibles en su archivo:\n  ",
    paste(names(datos), collapse = ", ")
  ))
}

# ── 4.4  Convertir fecha ─────────────────────────────────
if ("fecha" %in% names(datos)) {
  datos$fecha <- suppressWarnings(as.Date(datos$fecha))
}

# ── 4.5  Tratar valores faltantes ────────────────────────
na_total <- sum(is.na(datos))
if (na_total > 0) {
  cat(sprintf("  AVISO: %d valores faltantes detectados. Imputando...\n", na_total))
  for (col in names(datos)) {
    if (is.numeric(datos[[col]]) && any(is.na(datos[[col]])))
      datos[[col]][is.na(datos[[col]])] <- median(datos[[col]], na.rm = TRUE)
    else if (is.character(datos[[col]]) && any(is.na(datos[[col]]))) {
      moda <- names(sort(table(datos[[col]]), decreasing = TRUE))[1]
      datos[[col]][is.na(datos[[col]])] <- moda
    }
  }
}

# ── 4.6  Estandarizar texto en columnas clave ────────────
datos$equipo          <- trimws(datos$equipo)
datos$local_visitante <- trimws(tolower(datos$local_visitante))
datos$resultado       <- trimws(tolower(datos$resultado))
if ("rival"        %in% names(datos)) datos$rival        <- trimws(datos$rival)
if ("tipo_partido" %in% names(datos)) datos$tipo_partido <- trimws(tolower(datos$tipo_partido))
if ("clima"        %in% names(datos)) datos$clima        <- trimws(tolower(datos$clima))

# ── 4.7  Variables derivadas ─────────────────────────────
datos$es_local  <- as.integer(datos$local_visitante == "local")
datos$dif_goles <- datos$goles_favor - datos$goles_contra
datos$puntos    <- ifelse(datos$resultado == "victoria", 3L,
                  ifelse(datos$resultado == "empate",    1L, 0L))
datos$es_oficial <- if ("tipo_partido" %in% names(datos))
  as.integer(datos$tipo_partido != "amistoso") else 1L

# ── 4.8  Columnas opcionales: agregar con 0 si no existen
vars_opcionales <- c("tiros_puerta", "posesion", "lesionados",
                     "suspendidos", "temperatura", "humedad", "dias_descanso")
for (v in vars_opcionales) {
  if (!v %in% names(datos)) datos[[v]] <- 0
}

# Asegurar tipos numéricos
num_cols <- c("goles_favor","goles_contra","dif_goles","puntos","temperatura",
              "humedad","dias_descanso","tiros_puerta","posesion",
              "lesionados","suspendidos")
for (col in intersect(num_cols, names(datos))) {
  datos[[col]] <- suppressWarnings(as.numeric(datos[[col]]))
  datos[[col]][is.na(datos[[col]])] <- 0
}

# ── 4.9  Eliminar filas completamente duplicadas ─────────
n_antes   <- nrow(datos)
datos     <- datos[!duplicated(datos), ]
n_borradas <- n_antes - nrow(datos)
if (n_borradas > 0)
  cat(sprintf("  INFO: Se eliminaron %d filas duplicadas exactas.\n", n_borradas))

cat(sprintf("  Datos listos: %d registros únicos.\n\n", nrow(datos)))


# ============================================================
# BLOQUE 5 — SELECCIÓN INTERACTIVA DEL PARTIDO A PREDECIR
# ============================================================
# Se detectan todos los equipos disponibles en el archivo y
# se le pide al usuario que elija o escriba los nombres.

equipos_disponibles <- sort(unique(trimws(datos$equipo)))
n_equipos           <- length(equipos_disponibles)

cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  EQUIPOS DISPONIBLES EN EL ARCHIVO (%d equipos):\n", n_equipos))
cat("═══════════════════════════════════════════════════════════\n")

# Mostrar en columnas de 3
por_col <- 3
n_filas <- ceiling(n_equipos / por_col)
for (i in seq_len(n_filas)) {
  fila <- character(por_col)
  for (j in seq_len(por_col)) {
    idx <- (i - 1) * por_col + j
    if (idx <= n_equipos)
      fila[j] <- sprintf("  %2d. %-28s", idx, equipos_disponibles[idx])
    else
      fila[j] <- ""
  }
  cat(paste(fila, collapse = ""), "\n")
}
cat("\n")

# ── Función de selección: número o nombre ─────────────────
elegir_equipo <- function(prompt_txt) {
  repeat {
    entrada <- trimws(readline(prompt = prompt_txt))
    # Si ingresó un número
    num <- suppressWarnings(as.integer(entrada))
    if (!is.na(num) && num >= 1 && num <= n_equipos) {
      eq <- equipos_disponibles[num]
      cat(sprintf("    → Seleccionado: %s\n\n", eq))
      return(eq)
    }
    # Si ingresó texto exacto o parcial (insensible a mayúsculas)
    coincide <- equipos_disponibles[
      tolower(equipos_disponibles) == tolower(entrada)]
    if (length(coincide) == 0) {
      # Búsqueda parcial
      coincide <- equipos_disponibles[
        grepl(entrada, equipos_disponibles, ignore.case = TRUE)]
    }
    if (length(coincide) == 1) {
      cat(sprintf("    → Seleccionado: %s\n\n", coincide[1]))
      return(coincide[1])
    } else if (length(coincide) > 1) {
      cat("    Varios equipos coinciden:\n")
      for (k in seq_along(coincide)) cat(sprintf("      %d. %s\n", k, coincide[k]))
      cat("    Por favor ingrese el nombre exacto.\n\n")
    } else {
      cat(sprintf(
        "    No se encontró '%s'. Use el número de la lista o el nombre exacto.\n\n",
        entrada))
    }
  }
}

cat("───────────────────────────────────────────────────────────\n")
cat("  Ingrese el número o nombre del equipo (insensible a mayúsculas).\n")
cat("───────────────────────────────────────────────────────────\n\n")

equipo_local     <- elegir_equipo("  Equipo LOCAL     (número o nombre): ")
equipo_visitante <- elegir_equipo("  Equipo VISITANTE (número o nombre): ")

# ── Parámetros del partido ────────────────────────────────
cat("───────────────────────────────────────────────────────────\n")
cat("  CONDICIONES DEL PARTIDO\n")
cat("  (Presione Enter para aceptar el valor por defecto)\n")
cat("───────────────────────────────────────────────────────────\n\n")

# Climas disponibles en el archivo
if ("clima" %in% names(datos)) {
  climas_disponibles <- sort(unique(datos$clima[datos$clima != ""]))
  cat(sprintf("  Climas en los datos: %s\n", paste(climas_disponibles, collapse = ", ")))
}

leer_con_default <- function(prompt_txt, default_val) {
  entrada <- trimws(readline(prompt = sprintf("  %s [%s]: ", prompt_txt, default_val)))
  if (nchar(entrada) == 0) default_val else entrada
}
leer_num <- function(prompt_txt, default_val) {
  entrada <- leer_con_default(prompt_txt, as.character(default_val))
  num     <- suppressWarnings(as.numeric(entrada))
  if (is.na(num)) { cat("  Valor no válido, se usará el default.\n"); default_val } else num
}

clima_partido           <- leer_con_default("Clima del partido (Soleado/Nublado/Lluvia/Frio/Caluroso)", "Soleado")
temperatura_partido     <- leer_num("Temperatura en °C", 20)
humedad_partido         <- leer_num("Humedad en %", 60)
tipo_partido            <- leer_con_default("Tipo de partido (Liga/Copa/Eliminatoria/Oficial/Amistoso)", "Oficial")
dias_descanso_local     <- leer_num(paste("Días de descanso del", equipo_local), 7)
dias_descanso_visitante <- leer_num(paste("Días de descanso del", equipo_visitante), 7)

cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("  PARTIDO A PREDECIR:\n")
cat(sprintf("    Local     : %s\n", equipo_local))
cat(sprintf("    Visitante : %s\n", equipo_visitante))
cat(sprintf("    Tipo      : %s\n", tipo_partido))
cat(sprintf("    Clima     : %s | %.0f°C | Humedad %.0f%%\n",
            clima_partido, temperatura_partido, humedad_partido))
cat(sprintf("    Descanso  : Local %dd | Visitante %dd\n",
            dias_descanso_local, dias_descanso_visitante))
cat(sprintf("    Simulaciones: %s\n", format(n_simulaciones, big.mark = ",")))
cat("═══════════════════════════════════════════════════════════\n\n")


# ============================================================
# BLOQUE 6 — FILTRAR ÚLTIMOS N PARTIDOS DE CADA EQUIPO
# ============================================================

obtener_partidos <- function(datos, nombre_equipo, n = 10) {
  df <- datos[trimws(datos$equipo) == trimws(nombre_equipo), ]
  if (nrow(df) == 0)
    stop(sprintf("No se encontraron partidos para '%s'.", nombre_equipo))
  if ("fecha" %in% names(df) && !all(is.na(df$fecha)))
    df <- df[order(df$fecha, decreasing = TRUE), ]
  head(df, n)
}

datos_local     <- obtener_partidos(datos, equipo_local,     max_partidos_analizar)
datos_visitante <- obtener_partidos(datos, equipo_visitante, max_partidos_analizar)

cat(sprintf("  Partidos analizados → %-20s: %d\n", equipo_local,     nrow(datos_local)))
cat(sprintf("  Partidos analizados → %-20s: %d\n\n", equipo_visitante, nrow(datos_visitante)))


# ============================================================
# BLOQUE 7 — ESTADÍSTICAS DESCRIPTIVAS POR EQUIPO
# ============================================================

calcular_estadisticas <- function(df, nombre, dias_desc) {
  s <- list(nombre = nombre, n = nrow(df))

  s$gf_prom <- mean(df$goles_favor,  na.rm = TRUE)
  s$gc_prom <- mean(df$goles_contra, na.rm = TRUE)
  s$dg_prom <- mean(df$dif_goles,    na.rm = TRUE)

  s$victorias <- sum(df$resultado == "victoria")
  s$empates   <- sum(df$resultado == "empate")
  s$derrotas  <- sum(df$resultado == "derrota")

  ult5         <- head(df, 5)
  s$forma_pts  <- sum(ult5$puntos)
  s$forma_pct  <- s$forma_pts / 15

  local_df   <- df[df$es_local == 1, ]
  visit_df   <- df[df$es_local == 0, ]
  s$gf_local <- if (nrow(local_df) > 0) mean(local_df$goles_favor)  else s$gf_prom
  s$gc_local <- if (nrow(local_df) > 0) mean(local_df$goles_contra) else s$gc_prom
  s$gf_visit <- if (nrow(visit_df) > 0) mean(visit_df$goles_favor)  else s$gf_prom
  s$gc_visit <- if (nrow(visit_df) > 0) mean(visit_df$goles_contra) else s$gc_prom

  of_df        <- df[df$es_oficial == 1, ]
  s$gf_oficial <- if (nrow(of_df) > 0) mean(of_df$goles_favor) else s$gf_prom

  if ("clima" %in% names(df)) {
    clim_df    <- df[tolower(df$clima) == tolower(clima_partido), ]
    s$gf_clima <- if (nrow(clim_df) > 0) mean(clim_df$goles_favor)  else s$gf_prom
    s$gc_clima <- if (nrow(clim_df) > 0) mean(clim_df$goles_contra) else s$gc_prom
  } else {
    s$gf_clima <- s$gf_prom
    s$gc_clima <- s$gc_prom
  }

  res_vec    <- df$resultado
  s$racha_tipo <- res_vec[1]
  s$racha_n    <- 1L
  if (length(res_vec) > 1)
    for (i in 2:length(res_vec)) {
      if (res_vec[i] == s$racha_tipo) s$racha_n <- s$racha_n + 1L else break
    }

  s$rend_ofensivo  <- min(s$gf_prom / 3, 1)
  s$rend_defensivo <- 1 - min(s$gc_prom / 3, 1)

  s$dias_descanso <- dias_desc
  s$fatiga <- if (dias_desc < 3) 0.85 else if (dias_desc < 5) 0.93 else 1.00
  s
}

sL <- calcular_estadisticas(datos_local,     equipo_local,     dias_descanso_local)
sV <- calcular_estadisticas(datos_visitante, equipo_visitante, dias_descanso_visitante)

imprimir_stats <- function(s, rol) {
  cat(sprintf("\n  ── %s (%s) ────────────────────────────\n",
              toupper(s$nombre), rol))
  cat(sprintf("    Partidos analizados          : %d\n",      s$n))
  cat(sprintf("    Goles anotados (promedio)    : %.2f\n",    s$gf_prom))
  cat(sprintf("    Goles recibidos (promedio)   : %.2f\n",    s$gc_prom))
  cat(sprintf("    Victorias / Empates / Derrotas: %d/%d/%d\n",
              s$victorias, s$empates, s$derrotas))
  cat(sprintf("    Forma reciente (puntos/15)   : %d/15 (%.0f%%)\n",
              s$forma_pts, s$forma_pct * 100))
  cat(sprintf("    Racha actual                 : %d %s\n",
              s$racha_n, s$racha_tipo))
  cat(sprintf("    Rend. ofensivo / defensivo   : %.2f / %.2f\n",
              s$rend_ofensivo, s$rend_defensivo))
  cat(sprintf("    Factor fatiga                : %.2f (descanso %dd)\n",
              s$fatiga, s$dias_descanso))
}

cat("═══════════════════════════════════════════════════════════\n")
cat("  ESTADÍSTICAS DESCRIPTIVAS\n")
cat("═══════════════════════════════════════════════════════════")
imprimir_stats(sL, "LOCAL")
imprimir_stats(sV, "VISITANTE")
cat("\n")


# ============================================================
# BLOQUE 8 — ENFRENTAMIENTOS DIRECTOS (H2H)
# ============================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  HISTORIAL DE ENFRENTAMIENTOS DIRECTOS (H2H)\n")
cat("═══════════════════════════════════════════════════════════\n")

if ("rival" %in% names(datos)) {
  h2h <- datos[
    (trimws(datos$equipo) == trimws(equipo_local) &
     trimws(datos$rival)  == trimws(equipo_visitante)) |
    (trimws(datos$equipo) == trimws(equipo_visitante) &
     trimws(datos$rival)  == trimws(equipo_local)), ]

  if (nrow(h2h) > 0) {
    h2h_L    <- h2h[trimws(h2h$equipo) == trimws(equipo_local), ]
    h2h_V    <- h2h[trimws(h2h$equipo) == trimws(equipo_visitante), ]
    h2h_gf_L <- if (nrow(h2h_L) > 0) mean(h2h_L$goles_favor) else sL$gf_prom
    h2h_gf_V <- if (nrow(h2h_V) > 0) mean(h2h_V$goles_favor) else sV$gf_prom
    cat(sprintf("  Partidos H2H encontrados: %d\n", nrow(h2h)))
    cat(sprintf("  Prom. goles %-20s en H2H: %.2f\n", equipo_local, h2h_gf_L))
    cat(sprintf("  Prom. goles %-20s en H2H: %.2f\n", equipo_visitante, h2h_gf_V))
  } else {
    cat("  No se encontraron enfrentamientos directos en los datos.\n")
    h2h_gf_L <- sL$gf_prom
    h2h_gf_V <- sV$gf_prom
  }
} else {
  cat("  Columna 'rival' no encontrada. Se omite análisis H2H.\n")
  h2h_gf_L <- sL$gf_prom
  h2h_gf_V <- sV$gf_prom
}
cat("\n")


# ============================================================
# BLOQUE 9 — MODELO DE REGRESIÓN LINEAL
# ============================================================
# Variable dependiente: goles_favor
# Variables explicativas: seleccionadas dinámicamente según disponibilidad

cat("═══════════════════════════════════════════════════════════\n")
cat("  MODELO DE REGRESIÓN LINEAL\n")
cat("═══════════════════════════════════════════════════════════\n\n")

vars_cand <- c("es_local","dif_goles","puntos","es_oficial",
               "temperatura","humedad","dias_descanso",
               "tiros_puerta","posesion","lesionados","suspendidos")
vars_disp <- vars_cand[vars_cand %in% names(datos)]

datos_reg <- datos[, c("goles_favor", vars_disp), drop = FALSE]
datos_reg <- datos_reg[complete.cases(datos_reg), ]

vars_ok <- vars_disp[sapply(vars_disp, function(v) {
  is.numeric(datos_reg[[v]]) && var(datos_reg[[v]], na.rm = TRUE) > 1e-10
})]

if (length(vars_ok) == 0) {
  modelo_reg <- lm(goles_favor ~ 1, data = datos_reg)
} else {
  f_reg      <- as.formula(paste("goles_favor ~", paste(vars_ok, collapse = " + ")))
  modelo_reg <- lm(f_reg, data = datos_reg)
}

cat("  Resumen del modelo (variable dependiente: goles_favor):\n\n")
print(summary(modelo_reg))

cat("\n  ── INTERPRETACIÓN DE COEFICIENTES SIGNIFICATIVOS ───────\n")
coefs <- summary(modelo_reg)$coefficients
for (i in seq_len(nrow(coefs))) {
  nm  <- rownames(coefs)[i]
  b   <- coefs[i, 1]
  pv  <- coefs[i, 4]
  if (nm == "(Intercept)") {
    cat(sprintf("  • Intercepto: condiciones base → %.2f goles esperados.\n", b))
  } else if (pv < 0.10) {
    sig <- if (pv < 0.01) "(***)" else if (pv < 0.05) "(**)" else "(*)"
    dir <- if (b > 0) "AUMENTA" else "DISMINUYE"
    cat(sprintf("  • '%s': %s los goles en %.3f por unidad %s\n", nm, dir, abs(b), sig))
  }
}
r2 <- summary(modelo_reg)$r.squared
cat(sprintf("\n  R² = %.4f → el modelo explica el %.1f%% de la variabilidad.\n\n",
            r2, r2 * 100))


# ============================================================
# BLOQUE 10 — CÁLCULO DE GOLES ESPERADOS (LAMBDA DE POISSON)
# ============================================================
# λ_local y λ_visitante: tasa media de goles esperados.
# Se construye como media ponderada ajustada por factores externos.

cat("═══════════════════════════════════════════════════════════\n")
cat("  CÁLCULO DE GOLES ESPERADOS — PROCESO DE POISSON\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Factores de ajuste por condiciones externas
f_clima <- switch(tolower(clima_partido),
  "lluvia"   = 0.88,
  "frio"     = 0.92,
  "nublado"  = 0.97,
  "soleado"  = 1.00,
  "caluroso" = 0.93,   # calor extremo reduce el ritmo ofensivo
  1.00)                # default para cualquier otro valor

f_temp <- if (temperatura_partido < 5  | temperatura_partido > 35) 0.90 else
           if (temperatura_partido < 10 | temperatura_partido > 30) 0.95 else 1.00

f_hum  <- if (humedad_partido > 80) 0.93 else
           if (humedad_partido > 70) 0.97 else 1.00

f_tipo <- if (tolower(tipo_partido) == "amistoso") 0.90 else 1.00

# ── Lambda LOCAL ──────────────────────────────────────────
# Ponderación: 50% como local + 20% clima similar + 20% H2H + 10% oficial
base_L <- 0.50 * sL$gf_local  +
          0.20 * sL$gf_clima   +
          0.20 * h2h_gf_L      +
          0.10 * sL$gf_oficial

f_forma_L  <- 0.85 + 0.30 * sL$forma_pct
f_def_V    <- 1 - (sV$rend_defensivo - 0.50) * 0.30
bonus_loc  <- 0.18   # ventaja histórica de localía en fútbol

lambda_local <- max(0.30, (base_L + bonus_loc) * f_forma_L * f_def_V *
                           f_clima * f_temp * f_hum * f_tipo * sL$fatiga)

# ── Lambda VISITANTE ──────────────────────────────────────
base_V <- 0.50 * sV$gf_visit  +
          0.20 * sV$gf_clima   +
          0.20 * h2h_gf_V      +
          0.10 * sV$gf_oficial

f_forma_V <- 0.85 + 0.30 * sV$forma_pct
f_def_L   <- 1 - (sL$rend_defensivo - 0.50) * 0.30

lambda_visitante <- max(0.30, base_V * f_forma_V * f_def_L *
                              f_clima * f_temp * f_hum * f_tipo * sV$fatiga)

cat(sprintf("  λ LOCAL     [%s]: %.4f goles esperados\n",
            equipo_local, lambda_local))
cat(sprintf("  λ VISITANTE [%s]: %.4f goles esperados\n\n",
            equipo_visitante, lambda_visitante))

cat("  Factores de ajuste aplicados:\n")
cat(sprintf("    Clima %-10s: %.3f\n", clima_partido,    f_clima))
cat(sprintf("    Temperatura      : %.3f\n",             f_temp))
cat(sprintf("    Humedad          : %.3f\n",             f_hum))
cat(sprintf("    Tipo partido     : %.3f\n",             f_tipo))
cat(sprintf("    Fatiga local     : %.3f\n",             sL$fatiga))
cat(sprintf("    Fatiga visitante : %.3f\n",             sV$fatiga))
cat(sprintf("    Bonus localía    : +%.3f\n\n",         bonus_loc))


# ============================================================
# BLOQUE 11 — SIMULACIÓN MONTE CARLO (DISTRIBUCIÓN DE POISSON)
# ============================================================
# Se simulan n_simulaciones partidos con rpois(n, λ).
# Cada marcador observado → probabilidad estimada.

cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("  SIMULACIÓN MONTE CARLO — %s PARTIDOS\n",
            format(n_simulaciones, big.mark = ",")))
cat("═══════════════════════════════════════════════════════════\n\n")

set.seed(42)
goles_L_sim <- rpois(n_simulaciones, lambda_local)
goles_V_sim <- rpois(n_simulaciones, lambda_visitante)

tabla_marc <- as.data.frame(table(paste(goles_L_sim, goles_V_sim, sep = "-")),
                             stringsAsFactors = FALSE)
names(tabla_marc) <- c("Marcador", "Frecuencia")
tabla_marc$Probabilidad <- tabla_marc$Frecuencia / n_simulaciones
tabla_marc$Porcentaje   <- round(tabla_marc$Probabilidad * 100, 2)
tabla_marc <- tabla_marc[order(tabla_marc$Probabilidad, decreasing = TRUE), ]
rownames(tabla_marc) <- NULL


# ============================================================
# BLOQUE 12 — PROBABILIDAD MULTIVARIADA (ANALÍTICA)
# ============================================================
# Distribución conjunta: P(X=i, Y=j) = Pois(λL,i) × Pois(λV,j)
# Matriz de probabilidades para todos los marcadores posibles.

cat("═══════════════════════════════════════════════════════════\n")
cat("  DISTRIBUCIÓN CONJUNTA — PROBABILIDADES ANALÍTICAS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

max_g      <- 8
prob_matriz <- matrix(0, max_g + 1, max_g + 1,
                      dimnames = list(0:max_g, 0:max_g))
for (i in 0:max_g)
  for (j in 0:max_g)
    prob_matriz[i+1, j+1] <- dpois(i, lambda_local) * dpois(j, lambda_visitante)

p_local_win <- 0; p_empate <- 0; p_visit_win <- 0
for (i in 0:max_g) for (j in 0:max_g) {
  p <- prob_matriz[i+1, j+1]
  if (i > j) p_local_win <- p_local_win + p
  if (i == j) p_empate   <- p_empate   + p
  if (i < j) p_visit_win <- p_visit_win + p
}

p_over25 <- sum(sapply(0:max_g, function(i)
  sum(sapply(0:max_g, function(j)
    if ((i+j) > 2.5) prob_matriz[i+1,j+1] else 0))))
p_under25 <- 1 - p_over25

p_btts <- 1 - dpois(0, lambda_local) - dpois(0, lambda_visitante) +
              dpois(0, lambda_local) * dpois(0, lambda_visitante)

cat("  PROBABILIDADES DE RESULTADO:\n")
cat("  ────────────────────────────────────────────────\n")
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_local,     p_local_win * 100))
cat(sprintf("  Empate                               : %5.1f%%\n", p_empate    * 100))
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_visitante, p_visit_win * 100))
cat("  ────────────────────────────────────────────────\n")
cat(sprintf("  Over  2.5 goles                      : %5.1f%%\n", p_over25  * 100))
cat(sprintf("  Under 2.5 goles                      : %5.1f%%\n", p_under25 * 100))
cat(sprintf("  Ambos equipos anotan (BTTS)          : %5.1f%%\n", p_btts    * 100))
cat("  ────────────────────────────────────────────────\n\n")


# ============================================================
# BLOQUE 13 — ANÁLISIS DE CORRELACIONES
# ============================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  ANÁLISIS DE CORRELACIONES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

vars_cor <- intersect(
  c("goles_favor","goles_contra","es_local","dif_goles","puntos",
    "temperatura","humedad","dias_descanso","tiros_puerta",
    "posesion","lesionados","suspendidos"),
  names(datos))

datos_num <- datos[, vars_cor, drop = FALSE]
datos_num  <- datos_num[complete.cases(datos_num), ]
vars_var   <- vars_cor[sapply(vars_cor, function(v)
  is.numeric(datos_num[[v]]) && var(datos_num[[v]], na.rm = TRUE) > 1e-10)]
datos_num  <- datos_num[, vars_var, drop = FALSE]

mat_cor <- NULL
if (ncol(datos_num) >= 2) {
  mat_cor <- cor(datos_num, use = "complete.obs")
  cat("  Matriz de correlaciones:\n\n")
  print(round(mat_cor, 3))
  if ("goles_favor" %in% rownames(mat_cor)) {
    cat("\n  Correlaciones con 'goles_favor':\n")
    cor_gf <- sort(mat_cor["goles_favor", ], decreasing = TRUE)
    cor_gf <- cor_gf[names(cor_gf) != "goles_favor"]
    for (nm in names(cor_gf))
      cat(sprintf("    %-22s r = %+.3f\n", nm, cor_gf[nm]))
  }
} else {
  cat("  Insuficientes variables numéricas.\n")
}
cat("\n")


# ============================================================
# BLOQUE 14 — GRÁFICAS EN R STUDIO
# ============================================================

cat("═══════════════════════════════════════════════════════════\n")
cat("  GENERANDO GRÁFICAS...\n")
cat("═══════════════════════════════════════════════════════════\n\n")

COL_L  <- "#1B4F72"; COL_V  <- "#922B21"
COL_OK <- "#1ABC9C"; COL_EM <- "#F39C12"; COL_DE <- "#E74C3C"

col_res <- function(r) ifelse(r=="victoria", COL_OK, ifelse(r=="empate", COL_EM, COL_DE))

# ── G1/G2: Goles por partido ──────────────────────────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

graf_goles <- function(df, nombre, cf, cc) {
  n <- nrow(df); idx <- seq_len(n)
  ym <- max(c(df$goles_favor, df$goles_contra), na.rm = TRUE) + 1
  plot(idx, df$goles_favor, type="b", pch=19, lwd=2, col=cf,
       ylim=c(0,ym), xlab="Partido (1=más reciente)", ylab="Goles",
       main=paste("Goles —", nombre), xaxt="n")
  axis(1, at=idx, labels=idx)
  lines(idx, df$goles_contra, type="b", pch=17, col=cc, lwd=2, lty=2)
  abline(h=mean(df$goles_favor),  col=cf, lty=3, lwd=1.5)
  abline(h=mean(df$goles_contra), col=cc, lty=3, lwd=1.5)
  legend("topright", c("Favor","Contra"), col=c(cf,cc),
         lwd=2, pch=c(19,17), lty=c(1,2), bg="white", cex=0.85)
  grid(col="gray90")
}
graf_goles(datos_local,     equipo_local,     COL_L, COL_V)
graf_goles(datos_visitante, equipo_visitante, COL_V, COL_L)

# ── G3/G4: Forma reciente ─────────────────────────────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

graf_forma <- function(df, nombre) {
  n <- nrow(df)
  barplot(df$puntos, col=col_res(df$resultado), border="white",
          names.arg=paste0("P",seq_len(n)), ylim=c(0,3.8),
          main=paste("Forma reciente —", nombre),
          xlab="Partido", ylab="Puntos")
  legend("topright",
    c(sprintf("Victoria ×%d",sum(df$resultado=="victoria")),
      sprintf("Empate   ×%d",sum(df$resultado=="empate")),
      sprintf("Derrota  ×%d",sum(df$resultado=="derrota"))),
    fill=c(COL_OK,COL_EM,COL_DE), border=NA, bg="white", cex=0.85)
  grid(nx=NA, ny=NULL, col="gray90")
}
graf_forma(datos_local,     equipo_local)
graf_forma(datos_visitante, equipo_visitante)

# ── G5: Dispersión goles anotados vs recibidos ────────────
dev.new(width = 9, height = 7)
par(mar = c(5, 4, 4, 2))
df_disp <- rbind(
  data.frame(eq=equipo_local,     gf=datos_local$goles_favor,
             gc=datos_local$goles_contra,     cl=COL_L),
  data.frame(eq=equipo_visitante, gf=datos_visitante$goles_favor,
             gc=datos_visitante$goles_contra, cl=COL_V))
plot(df_disp$gf, df_disp$gc, col=df_disp$cl, pch=19, cex=1.6,
     xlab="Goles anotados", ylab="Goles recibidos",
     main="Goles anotados vs. Goles recibidos")
tryCatch({
  dL2 <- df_disp[df_disp$eq==equipo_local,]
  dV2 <- df_disp[df_disp$eq==equipo_visitante,]
  if(var(dL2$gf)>0) abline(lm(gc~gf,dL2), col=COL_L, lwd=2, lty=2)
  if(var(dV2$gf)>0) abline(lm(gc~gf,dV2), col=COL_V, lwd=2, lty=2)
}, error=function(e) NULL)
legend("topright", c(equipo_local, equipo_visitante),
       col=c(COL_L,COL_V), pch=19, lty=2, lwd=2, bg="white")
grid(col="gray90")

# ── G6/G7: Histogramas goles simulados ───────────────────
dev.new(width = 14, height = 6)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

graf_hist <- function(sim, lam, nombre, col_eq) {
  brk <- seq(-0.5, max(sim)+0.5, 1)
  hist(sim, breaks=brk, col=col_eq, border="white", freq=FALSE,
       xlab="Goles", ylab="Densidad",
       main=paste("Distribución simulada —", nombre),
       xlim=c(-0.5, min(max(sim)+0.5, 10)))
  pts <- 0:min(max(sim),10)
  points(pts, dpois(pts, lam), type="b", pch=19, col="white", lwd=2)
  abline(v=lam, col="yellow", lwd=2, lty=2)
  legend("topright", c("Simulación","Teórico",sprintf("λ=%.2f",lam)),
         col=c(col_eq,"white","yellow"), pch=c(15,19,NA),
         lty=c(NA,1,2), lwd=2, bg="gray20", text.col="white", cex=0.85)
}
graf_hist(goles_L_sim, lambda_local,     equipo_local,     COL_L)
graf_hist(goles_V_sim, lambda_visitante, equipo_visitante, COL_V)

# ── G8: Barras top 10 marcadores ─────────────────────────
dev.new(width = 11, height = 7)
par(mar = c(5, 5, 4, 2))
top10  <- head(tabla_marc, 10)
grad10 <- colorRampPalette(c(COL_L, "#7F8C8D", COL_V))(nrow(top10))
bp8 <- barplot(top10$Porcentaje, names.arg=as.character(top10$Marcador),
               col=grad10, border="white",
               main=paste("Top 10 marcadores más probables\n",
                          equipo_local,"vs",equipo_visitante),
               xlab="Marcador", ylab="Probabilidad (%)",
               ylim=c(0, max(top10$Porcentaje)*1.25))
text(bp8, top10$Porcentaje + max(top10$Porcentaje)*0.04,
     paste0(top10$Porcentaje,"%"), cex=0.9, font=2)
grid(nx=NA, ny=NULL, col="gray85")

# ── G9: Mapa de calor de probabilidades ──────────────────
max_hm <- min(5, max_g)
hm_df  <- reshape2::melt(prob_matriz[1:(max_hm+1), 1:(max_hm+1)])
names(hm_df) <- c("GL","GV","P")
hm_df$GL <- as.integer(hm_df$GL) - 1L
hm_df$GV <- as.integer(hm_df$GV) - 1L
hm_df$Lbl <- paste0(round(hm_df$P*100,1),"%")

print(
  ggplot(hm_df, aes(x=factor(GV), y=factor(GL), fill=P)) +
    geom_tile(color="white", linewidth=0.6) +
    geom_text(aes(label=Lbl), color="white", size=3.5, fontface="bold") +
    scale_fill_gradient2(low="#154360", mid="#2E86C1", high="#E74C3C",
                         midpoint=median(hm_df$P), name="P(marcador)") +
    scale_x_discrete(name=paste("Goles", equipo_visitante)) +
    scale_y_discrete(name=paste("Goles", equipo_local)) +
    labs(title=paste("Mapa de calor — Probabilidad de marcadores\n",
                     equipo_local,"vs",equipo_visitante)) +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(hjust=0.5, face="bold"),
          panel.grid=element_blank())
)

# ── G10: Matriz de correlaciones ─────────────────────────
if (!is.null(mat_cor) && nrow(mat_cor) >= 2) {
  dev.new(width = 9, height = 8)
  corrplot::corrplot(mat_cor, method="color", type="upper",
                     tl.col="black", tl.srt=45, tl.cex=0.85,
                     addCoef.col="black", number.cex=0.70,
                     col=colorRampPalette(c(COL_V,"white",COL_L))(200),
                     title="Matriz de Correlaciones", mar=c(0,0,2,0))
}

# ── G11: Diagnóstico de la regresión ─────────────────────
dev.new(width = 12, height = 10)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
plot(modelo_reg, which=c(1,2,3,5),
     sub.caption="Diagnóstico — Variable: goles_favor")

# ── G12: Comparativa estadísticas ────────────────────────
dev.new(width = 13, height = 6)
par(mar = c(6, 4, 4, 2))
vcomp  <- c("Goles\nanot.","Goles\nrecib.","Forma\n/15","R.ofens\n×3","R.defens\n×3")
vL_bar <- c(sL$gf_prom, sL$gc_prom, sL$forma_pts, sL$rend_ofensivo*3, sL$rend_defensivo*3)
vV_bar <- c(sV$gf_prom, sV$gc_prom, sV$forma_pts, sV$rend_ofensivo*3, sV$rend_defensivo*3)
barplot(rbind(vL_bar,vV_bar), beside=TRUE, names.arg=vcomp,
        col=c(COL_L,COL_V), border="white",
        main=paste("Comparativa —", equipo_local,"vs",equipo_visitante),
        ylab="Valor", cex.names=0.85)
legend("topright", c(equipo_local,equipo_visitante),
       fill=c(COL_L,COL_V), border=NA, bg="white")
grid(nx=NA, ny=NULL, col="gray85")

# ── G13: Probabilidades de resultado ─────────────────────
dev.new(width = 9, height = 7)
par(mar = c(5, 4, 4, 2))
pres <- c(p_local_win, p_empate, p_visit_win)*100
nres <- c(paste("Victoria\n",equipo_local), "Empate",
           paste("Victoria\n",equipo_visitante))
bp13 <- barplot(pres, names.arg=nres, col=c(COL_L,COL_EM,COL_V),
                border="white", ylim=c(0,max(pres)*1.30),
                main=paste("Probabilidades de resultado\n",
                           equipo_local,"vs",equipo_visitante),
                ylab="Probabilidad (%)")
text(bp13, pres+max(pres)*0.05, paste0(round(pres,1),"%"), cex=1.2, font=2)
grid(nx=NA, ny=NULL, col="gray85")

cat("  Gráficas generadas. Revise el panel 'Plots' de RStudio.\n\n")


# ============================================================
# BLOQUE 15 — RESUMEN FINAL Y CONCLUSIONES
# ============================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════╗\n")
cat("║              RESUMEN FINAL DE PREDICCIÓN                 ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("  %s  vs  %s\n", equipo_local, equipo_visitante))
cat(sprintf("  %s | %s | %.0f°C | Humedad %.0f%%\n\n",
            tipo_partido, clima_partido, temperatura_partido, humedad_partido))

cat("  ── GOLES ESPERADOS ─────────────────────────────────────\n")
cat(sprintf("    %-25s: %.3f\n", equipo_local,     lambda_local))
cat(sprintf("    %-25s: %.3f\n", equipo_visitante, lambda_visitante))
cat(sprintf("    Total goles esperados        : %.3f\n\n",
            lambda_local + lambda_visitante))

cat("  ── MARCADOR MÁS PROBABLE ───────────────────────────────\n")
cat(sprintf("    >>> %s   con   %.2f%% <<<\n\n",
            tabla_marc$Marcador[1], tabla_marc$Porcentaje[1]))

cat("  ── TOP 10 MARCADORES ───────────────────────────────────\n")
cat(sprintf("  %-10s  %-12s  %s\n", "Marcador","Probabilidad","Barra"))
cat("  ──────────────────────────────────────────────────────\n")
for (i in seq_len(min(10, nrow(tabla_marc)))) {
  barra <- paste(rep("█", max(1, round(tabla_marc$Porcentaje[i]))), collapse="")
  cat(sprintf("  %-10s  %6.2f%%       %s\n",
              tabla_marc$Marcador[i], tabla_marc$Porcentaje[i], barra))
}

cat("\n  ── PROBABILIDADES DE RESULTADO ─────────────────────────\n")
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_local,     p_local_win*100))
cat(sprintf("  Empate                               : %5.1f%%\n", p_empate*100))
cat(sprintf("  Victoria %-22s: %5.1f%%\n", equipo_visitante, p_visit_win*100))

cat("\n  ── MERCADOS ADICIONALES ────────────────────────────────\n")
cat(sprintf("  Over  2.5 goles                      : %5.1f%%\n", p_over25*100))
cat(sprintf("  Under 2.5 goles                      : %5.1f%%\n", p_under25*100))
cat(sprintf("  Ambos equipos anotan (BTTS)          : %5.1f%%\n", p_btts*100))

cat("\n  ── INTERPRETACIÓN AUTOMÁTICA ───────────────────────────\n\n")

if (p_local_win > p_visit_win + 0.10)
  cat(sprintf("  ▶ FAVORITO: %s (LOCAL) por %.1f pp.\n",
              equipo_local, (p_local_win-p_visit_win)*100))
else if (p_visit_win > p_local_win + 0.10)
  cat(sprintf("  ▶ FAVORITO: %s (VISITANTE) por %.1f pp.\n",
              equipo_visitante, (p_visit_win-p_local_win)*100))
else
  cat("  ▶ PARTIDO MUY EQUILIBRADO — sin favorito claro.\n")

tot <- lambda_local + lambda_visitante
cat(sprintf("  ▶ TIPO: %s\n",
  if (tot > 3.5) "Partido ABIERTO — goleada probable." else
  if (tot > 2.5) "Partido con VARIOS GOLES esperados." else
  if (tot > 1.5) "Partido EQUILIBRADO con pocos goles." else
  "Partido MUY CERRADO y defensivo."))

if (sL$forma_pct > sV$forma_pct + 0.2)
  cat(sprintf("  ▶ FORMA: %s llega en mejor estado (%d vs %d pts).\n",
              equipo_local, sL$forma_pts, sV$forma_pts))
else if (sV$forma_pct > sL$forma_pct + 0.2)
  cat(sprintf("  ▶ FORMA: %s llega en mejor estado (%d vs %d pts).\n",
              equipo_visitante, sV$forma_pts, sL$forma_pts))
else
  cat(sprintf("  ▶ FORMA: Ambos equipos similares (%d vs %d pts).\n",
              sL$forma_pts, sV$forma_pts))

if (f_clima < 0.95)
  cat(sprintf("  ▶ CLIMA: El %s reduce el ritmo ofensivo (factor %.2f).\n",
              clima_partido, f_clima))
if (sL$fatiga < 1)
  cat(sprintf("  ▶ FATIGA: %s con solo %dd de descanso (factor %.2f).\n",
              equipo_local, dias_descanso_local, sL$fatiga))
if (sV$fatiga < 1)
  cat(sprintf("  ▶ FATIGA: %s con solo %dd de descanso (factor %.2f).\n",
              equipo_visitante, dias_descanso_visitante, sV$fatiga))

cat("\n")
cat("  ╔───────────────────────────────────────────────────────╗\n")
cat("  ║  NOTA: Este modelo estima PROBABILIDADES basadas en   ║\n")
cat("  ║  datos históricos. No predice el resultado real.      ║\n")
cat("  ║  El fútbol tiene varianza aleatoria inherente.        ║\n")
cat("  ╚───────────────────────────────────────────────────────╝\n\n")

cat("  Análisis completado. Revise las gráficas en RStudio.\n\n")
