# ╔══════════════════════════════════════════════════════════════╗
# ║   PREDICTOR DE MARCADORES DE FÚTBOL — MUNDIAL 2026          ║
# ║   Modelo: Regresión Lineal + Poisson + Probabilidad Multi.  ║
# ╚══════════════════════════════════════════════════════════════╝
#
# INSTRUCCIONES:
#  1. Ejecute PRIMERO solo el BLOQUE 1 y BLOQUE 2 (seleccione
#     esas líneas y presione Ctrl+Enter) para ver los equipos.
#  2. Edite la sección "★ CONFIGURE AQUÍ ★" con los equipos
#     y condiciones del partido que desea predecir.
#  3. Seleccione TODO (Ctrl+A) y ejecute (Ctrl+Enter).
#
# EQUIPOS DISPONIBLES EN EL ARCHIVO:
#  Alemania, Arabia Saudita, Argelia, Argentina, Australia,
#  Austria, Bosnia y Herzegovina, Brasil, Bélgica, Cabo Verde,
#  Canadá, Catar, Colombia, Corea del Sur, Costa de Marfil,
#  Croacia, Curazao, Ecuador, Egipto, Escocia, España,
#  Estados Unidos, Francia, Ghana, Haití, Inglaterra, Irak,
#  Irán, Japón, Jordania, Marruecos, México, Noruega,
#  Nueva Zelanda, Panamá, Paraguay, Países Bajos, Portugal,
#  República Checa, República Democrática del Congo, Senegal,
#  Sudáfrica, Suecia, Suiza, Turquía, Túnez, Uruguay, Uzbekistán
#
# CLIMAS VÁLIDOS : Soleado | Nublado | Lluvia | Frio | Caluroso
# TIPOS VÁLIDOS  : Oficial | Eliminatoria | Copa | Liga de Naciones | Amistoso
# ══════════════════════════════════════════════════════════════════


# ══════════════════════════════════════════════════════════════════
# ★★★ CONFIGURE AQUÍ — EDITE SOLO ESTA SECCIÓN ★★★
# ══════════════════════════════════════════════════════════════════

EQUIPO_LOCAL      <- "Argentina"       # Escriba el nombre EXACTO del equipo local
EQUIPO_VISITANTE  <- "Francia"         # Escriba el nombre EXACTO del equipo visitante

CLIMA             <- "Soleado"         # Soleado | Nublado | Lluvia | Frio | Caluroso
TEMPERATURA       <- 22                # Temperatura en grados Celsius
HUMEDAD           <- 60                # Humedad en % (0-100)
TIPO_PARTIDO      <- "Oficial"         # Oficial | Eliminatoria | Copa | Liga de Naciones | Amistoso
DESCANSO_LOCAL    <- 7                 # Días de descanso del equipo local
DESCANSO_VISITANTE <- 7                # Días de descanso del equipo visitante

N_SIMULACIONES    <- 10000             # Simulaciones Monte Carlo (no cambiar)
N_PARTIDOS        <- 10                # Últimos N partidos a analizar por equipo

# ══════════════════════════════════════════════════════════════════
# FIN DE LA CONFIGURACIÓN — No modifique el resto del código
# ══════════════════════════════════════════════════════════════════


# ─────────────────────────────────────────────────────────────────
# BLOQUE 1 — LIBRERÍAS
# ─────────────────────────────────────────────────────────────────
# Se instalan y cargan automáticamente todos los paquetes necesarios.

pkgs <- c("readxl","ggplot2","corrplot","reshape2","scales","RColorBrewer")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, dependencies = TRUE, quiet = TRUE)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}

cat("\n══════════════════════════════════════════════════════════════\n")
cat("   PREDICTOR DE MARCADORES DE FÚTBOL — MUNDIAL 2026\n")
cat("══════════════════════════════════════════════════════════════\n\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 2 — CARGA DEL ARCHIVO CSV
# ─────────────────────────────────────────────────────────────────
# Se abre el explorador de archivos para seleccionar el CSV.
# Acepte también .xlsx si su archivo es Excel.

cat(">>> Seleccione el archivo CSV del Mundial...\n\n")
ruta_archivo <- file.choose()
ext          <- tolower(tools::file_ext(ruta_archivo))

if (ext == "csv") {
  datos_raw <- read.csv(ruta_archivo, stringsAsFactors = FALSE,
                        encoding = "UTF-8", na.strings = c("","NA","N/A"))
} else if (ext %in% c("xlsx","xls")) {
  datos_raw <- as.data.frame(readxl::read_excel(ruta_archivo))
} else {
  stop("Solo se aceptan archivos .csv o .xlsx")
}

cat(sprintf("  Archivo cargado: %d filas × %d columnas\n\n",
            nrow(datos_raw), ncol(datos_raw)))

# Mostrar equipos disponibles
equipos_raw <- sort(unique(trimws(datos_raw[[1]])))
cat("  EQUIPOS DISPONIBLES EN EL ARCHIVO:\n")
cat("  ─────────────────────────────────────────────────────\n")
n_eq  <- length(equipos_raw)
n_col <- 3
for (i in seq(1, n_eq, by = n_col)) {
  fila <- sprintf("  %2d.%-26s", i, equipos_raw[i])
  if (i+1 <= n_eq) fila <- paste0(fila, sprintf("%2d.%-26s", i+1, equipos_raw[i+1]))
  if (i+2 <= n_eq) fila <- paste0(fila, sprintf("%2d.%s",     i+2, equipos_raw[i+2]))
  cat(fila, "\n")
}
cat("\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 3 — LIMPIEZA Y PREPARACIÓN DE DATOS
# ─────────────────────────────────────────────────────────────────
# Se estandarizan columnas, se eliminan duplicados y se crean
# las variables derivadas que necesita el modelo estadístico.

datos <- datos_raw

# 3.1 Estandarizar nombres de columnas (minúsculas, sin espacios)
names(datos) <- tolower(trimws(gsub("[^a-zA-Z0-9]", "_", names(datos))))

# 3.2 Eliminar filas completamente duplicadas
n_antes <- nrow(datos)
datos   <- datos[!duplicated(datos), ]
cat(sprintf("  Filas duplicadas eliminadas: %d → quedan %d registros únicos.\n\n",
            n_antes - nrow(datos), nrow(datos)))

# 3.3 Estandarizar texto en columnas clave
datos$equipo          <- trimws(datos$equipo)
datos$rival           <- trimws(datos$rival)
datos$local_visitante <- trimws(tolower(datos$local_visitante))
datos$resultado       <- trimws(tolower(datos$resultado))
datos$tipo_partido    <- trimws(datos$tipo_partido)   # conserva capitalización original
datos$clima           <- trimws(datos$clima)          # conserva capitalización original

# 3.4 Convertir fecha
datos$fecha <- suppressWarnings(as.Date(datos$fecha))

# 3.5 Convertir columnas numéricas
num_cols <- c("goles_favor","goles_contra","temperatura","humedad",
              "dias_descanso","tiros_puerta","posesion","lesionados","suspendidos")
for (col in num_cols) {
  if (col %in% names(datos)) {
    datos[[col]] <- suppressWarnings(as.numeric(datos[[col]]))
    datos[[col]][is.na(datos[[col]])] <- median(datos[[col]], na.rm = TRUE)
  }
}

# 3.6 Variables derivadas
datos$es_local   <- as.integer(datos$local_visitante == "local")
datos$dif_goles  <- datos$goles_favor - datos$goles_contra
datos$puntos     <- ifelse(datos$resultado == "victoria", 3L,
                   ifelse(datos$resultado == "empate",    1L, 0L))
# Oficial = todo lo que NO es Amistoso
datos$es_oficial <- as.integer(tolower(datos$tipo_partido) != "amistoso")

# 3.7 Revisar valores faltantes
na_total <- sum(is.na(datos[, num_cols[num_cols %in% names(datos)]]))
if (na_total > 0) {
  cat(sprintf("  AVISO: %d valores faltantes imputados con la mediana.\n\n", na_total))
} else {
  cat("  Sin valores faltantes. Datos completos.\n\n")
}

cat("  Vista previa de los datos limpios:\n")
print(head(datos[, c("equipo","rival","fecha","local_visitante",
                     "goles_favor","goles_contra","resultado",
                     "tipo_partido","clima","temperatura","humedad",
                     "dias_descanso","tiros_puerta","posesion",
                     "lesionados","suspendidos")], 4))
cat("\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 4 — VALIDAR EQUIPOS Y FILTRAR DATOS
# ─────────────────────────────────────────────────────────────────

equipos_disponibles <- sort(unique(datos$equipo))

validar_equipo <- function(nombre) {
  if (!nombre %in% equipos_disponibles) {
    # Búsqueda parcial por si hay error tipográfico menor
    parcial <- equipos_disponibles[grepl(nombre, equipos_disponibles, ignore.case = TRUE)]
    if (length(parcial) == 1) {
      cat(sprintf("  AVISO: '%s' no encontrado. Usando '%s'.\n", nombre, parcial[1]))
      return(parcial[1])
    }
    stop(sprintf(
      "\n  ERROR: Equipo '%s' no encontrado.\n  Equipos disponibles:\n    %s\n",
      nombre, paste(equipos_disponibles, collapse = "\n    ")
    ))
  }
  nombre
}

EQUIPO_LOCAL     <- validar_equipo(EQUIPO_LOCAL)
EQUIPO_VISITANTE <- validar_equipo(EQUIPO_VISITANTE)

# Filtrar últimos N partidos de cada equipo (ordenados por fecha desc)
filtrar_equipo <- function(df, equipo, n) {
  d <- df[df$equipo == equipo, ]
  if (!all(is.na(d$fecha))) d <- d[order(d$fecha, decreasing = TRUE), ]
  head(d, n)
}

dL <- filtrar_equipo(datos, EQUIPO_LOCAL,     N_PARTIDOS)
dV <- filtrar_equipo(datos, EQUIPO_VISITANTE, N_PARTIDOS)

cat(sprintf("  Partidos cargados: %s → %d | %s → %d\n\n",
            EQUIPO_LOCAL, nrow(dL), EQUIPO_VISITANTE, nrow(dV)))


# ─────────────────────────────────────────────────────────────────
# BLOQUE 5 — ESTADÍSTICAS DESCRIPTIVAS POR EQUIPO
# ─────────────────────────────────────────────────────────────────
# Se calculan todos los indicadores de rendimiento usando las
# 16 columnas del archivo para construir los lambdas de Poisson.

calc_stats <- function(df, nombre, dias_desc) {
  s <- list(nombre = nombre, n = nrow(df))

  # ── Promedios globales ────────────────────────────────────
  s$gf       <- mean(df$goles_favor,  na.rm = TRUE)
  s$gc       <- mean(df$goles_contra, na.rm = TRUE)
  s$dg       <- mean(df$dif_goles,    na.rm = TRUE)
  s$tp       <- mean(df$tiros_puerta, na.rm = TRUE)   # tiros a puerta
  s$pos      <- mean(df$posesion,     na.rm = TRUE)   # posesión
  s$lesion   <- mean(df$lesionados,   na.rm = TRUE)   # lesionados promedio
  s$suspend  <- mean(df$suspendidos,  na.rm = TRUE)   # suspendidos promedio

  # ── Resultados ────────────────────────────────────────────
  s$vic <- sum(df$resultado == "victoria")
  s$emp <- sum(df$resultado == "empate")
  s$der <- sum(df$resultado == "derrota")

  # ── Forma reciente: puntos de los últimos 5 partidos ──────
  u5          <- head(df, 5)
  s$forma_pts <- sum(u5$puntos)         # máximo posible: 15
  s$forma_pct <- s$forma_pts / 15       # normalizado 0-1

  # ── Rendimiento como Local ────────────────────────────────
  dL_  <- df[df$es_local == 1, ]
  s$gf_local <- if (nrow(dL_) > 0) mean(dL_$goles_favor)  else s$gf
  s$gc_local <- if (nrow(dL_) > 0) mean(dL_$goles_contra) else s$gc

  # ── Rendimiento como Visitante ────────────────────────────
  dV_  <- df[df$es_local == 0, ]
  s$gf_visit <- if (nrow(dV_) > 0) mean(dV_$goles_favor)  else s$gf
  s$gc_visit <- if (nrow(dV_) > 0) mean(dV_$goles_contra) else s$gc

  # ── Rendimiento en partidos oficiales (no amistosos) ──────
  dOf_ <- df[df$es_oficial == 1, ]
  s$gf_oficial <- if (nrow(dOf_) > 0) mean(dOf_$goles_favor) else s$gf

  # ── Rendimiento en clima similar al partido ───────────────
  dClim_ <- df[tolower(df$clima) == tolower(CLIMA), ]
  s$gf_clima <- if (nrow(dClim_) > 0) mean(dClim_$goles_favor)  else s$gf
  s$gc_clima <- if (nrow(dClim_) > 0) mean(dClim_$goles_contra) else s$gc

  # ── Rendimiento con temperatura similar (±5°C) ───────────
  dTemp_ <- df[abs(df$temperatura - TEMPERATURA) <= 5, ]
  s$gf_temp <- if (nrow(dTemp_) > 0) mean(dTemp_$goles_favor) else s$gf

  # ── Racha actual ──────────────────────────────────────────
  rv <- df$resultado
  s$racha_tipo <- rv[1]
  s$racha_n    <- 1L
  if (length(rv) > 1)
    for (i in 2:length(rv)) {
      if (rv[i] == s$racha_tipo) s$racha_n <- s$racha_n + 1L else break
    }

  # ── Índices de nivel ofensivo y defensivo (0-1) ───────────
  # Se usa el promedio ponderado: goles + tiros a puerta
  s$nivel_ofensivo  <- min((s$gf * 0.7 + s$tp * 0.03), 1)
  s$nivel_defensivo <- 1 - min((s$gc * 0.7 + (10 - min(s$tp, 10)) * 0.03), 1)
  s$nivel_ofensivo  <- max(0, min(1, s$nivel_ofensivo))
  s$nivel_defensivo <- max(0, min(1, s$nivel_defensivo))

  # ── Factor de fatiga (días de descanso) ───────────────────
  s$descanso <- dias_desc
  s$fatiga   <- if (dias_desc < 3) 0.85 else if (dias_desc < 5) 0.93 else 1.00

  # ── Impacto de lesionados y suspendidos ───────────────────
  # Cada jugador clave ausente reduce ligeramente el rendimiento
  bajas_actuales <- df$lesionados[1] + df$suspendidos[1]
  s$factor_bajas <- max(0.80, 1 - bajas_actuales * 0.02)

  s
}

sL <- calc_stats(dL, EQUIPO_LOCAL,     DESCANSO_LOCAL)
sV <- calc_stats(dV, EQUIPO_VISITANTE, DESCANSO_VISITANTE)

# Imprimir estadísticas
print_stats <- function(s, rol) {
  cat(sprintf("\n  ── %s [%s] ─────────────────────────────\n",
              toupper(s$nombre), rol))
  cat(sprintf("    Partidos analizados              : %d\n",      s$n))
  cat(sprintf("    Goles anotados (promedio)        : %.2f\n",    s$gf))
  cat(sprintf("    Goles recibidos (promedio)       : %.2f\n",    s$gc))
  cat(sprintf("    Diferencia de goles (promedio)   : %+.2f\n",  s$dg))
  cat(sprintf("    Tiros a puerta (promedio)        : %.1f\n",    s$tp))
  cat(sprintf("    Posesión (promedio)              : %.1f%%\n",  s$pos))
  cat(sprintf("    Victorias / Empates / Derrotas   : %d/%d/%d\n",
              s$vic, s$emp, s$der))
  cat(sprintf("    Forma reciente (puntos/15)       : %d/15 → %.0f%%\n",
              s$forma_pts, s$forma_pct*100))
  cat(sprintf("    Racha actual                     : %d x %s\n",
              s$racha_n, s$racha_tipo))
  cat(sprintf("    Nivel ofensivo (0-1)             : %.3f\n",   s$nivel_ofensivo))
  cat(sprintf("    Nivel defensivo (0-1)            : %.3f\n",   s$nivel_defensivo))
  cat(sprintf("    Factor fatiga                    : %.2f (%dd descanso)\n",
              s$fatiga, s$descanso))
  cat(sprintf("    Factor bajas (lesion+suspend.)   : %.2f\n",   s$factor_bajas))
  cat(sprintf("    Goles como %s (prom)          : %.2f\n",
              rol, if (rol=="LOCAL") s$gf_local else s$gf_visit))
  cat(sprintf("    Goles en clima '%s'           : %.2f\n",
              CLIMA, s$gf_clima))
}

cat("══════════════════════════════════════════════════════════════\n")
cat("  ESTADÍSTICAS DESCRIPTIVAS\n")
cat("══════════════════════════════════════════════════════════════")
print_stats(sL, "LOCAL")
print_stats(sV, "VISITANTE")
cat("\n\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 6 — ENFRENTAMIENTOS DIRECTOS (HEAD TO HEAD)
# ─────────────────────────────────────────────────────────────────
# Se buscan todos los partidos previos entre ambos equipos en el
# dataset y se calculan sus promedios de goles.

cat("══════════════════════════════════════════════════════════════\n")
cat("  ENFRENTAMIENTOS DIRECTOS (H2H)\n")
cat("══════════════════════════════════════════════════════════════\n")

h2h <- datos[
  (datos$equipo == EQUIPO_LOCAL     & datos$rival == EQUIPO_VISITANTE) |
  (datos$equipo == EQUIPO_VISITANTE & datos$rival == EQUIPO_LOCAL),
]

if (nrow(h2h) > 0) {
  h2h_dL      <- h2h[h2h$equipo == EQUIPO_LOCAL, ]
  h2h_dV      <- h2h[h2h$equipo == EQUIPO_VISITANTE, ]
  h2h_gf_L    <- if (nrow(h2h_dL) > 0) mean(h2h_dL$goles_favor)  else sL$gf
  h2h_gf_V    <- if (nrow(h2h_dV) > 0) mean(h2h_dV$goles_favor)  else sV$gf
  h2h_gc_L    <- if (nrow(h2h_dL) > 0) mean(h2h_dL$goles_contra) else sL$gc
  h2h_gc_V    <- if (nrow(h2h_dV) > 0) mean(h2h_dV$goles_contra) else sV$gc
  h2h_total   <- mean(h2h$goles_favor + h2h$goles_contra)

  cat(sprintf("  Partidos H2H en el dataset      : %d\n",     nrow(h2h)))
  cat(sprintf("  Prom. goles anotados %-12s: %.2f\n",
              EQUIPO_LOCAL,     h2h_gf_L))
  cat(sprintf("  Prom. goles anotados %-12s: %.2f\n",
              EQUIPO_VISITANTE, h2h_gf_V))
  cat(sprintf("  Prom. goles totales por partido : %.2f\n",   h2h_total))
  hay_h2h <- TRUE
} else {
  cat("  No se encontraron H2H en el dataset. Se usan stats individuales.\n")
  h2h_gf_L <- sL$gf;  h2h_gf_V <- sV$gf
  h2h_gc_L <- sL$gc;  h2h_gc_V <- sV$gc
  hay_h2h  <- FALSE
}
cat("\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 7 — MODELO DE REGRESIÓN LINEAL
# ─────────────────────────────────────────────────────────────────
# Variable dependiente : goles_favor (goles anotados)
# Variables explicativas: todas las columnas numéricas disponibles
#
# El modelo OLS (mínimos cuadrados) estima:
#   goles = β0 + β1·es_local + β2·temperatura + β3·humedad +
#           β4·dias_descanso + β5·tiros_puerta + β6·posesion +
#           β7·lesionados + β8·suspendidos + β9·es_oficial + ε
#
# summary() muestra coeficientes, p-valores y R².
# Se interpreta automáticamente cada coeficiente significativo.

cat("══════════════════════════════════════════════════════════════\n")
cat("  MODELO DE REGRESIÓN LINEAL\n")
cat("  Variable dependiente: goles_favor\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Variables predictoras del dataset completo
vars_reg <- c("es_local","temperatura","humedad","dias_descanso",
              "tiros_puerta","posesion","lesionados","suspendidos","es_oficial")
vars_reg <- vars_reg[vars_reg %in% names(datos)]

datos_m <- datos[, c("goles_favor", vars_reg)]
datos_m <- datos_m[complete.cases(datos_m), ]

# Eliminar columnas con varianza cero (constantes)
vars_ok <- vars_reg[sapply(vars_reg, function(v)
  is.numeric(datos_m[[v]]) && var(datos_m[[v]], na.rm = TRUE) > 1e-10)]

formula_reg <- as.formula(paste("goles_favor ~", paste(vars_ok, collapse = " + ")))
modelo_reg  <- lm(formula_reg, data = datos_m)

cat("  Resumen del modelo:\n\n")
print(summary(modelo_reg))

cat("\n  ── INTERPRETACIÓN DE COEFICIENTES ──────────────────────\n")
cf <- summary(modelo_reg)$coefficients
for (i in seq_len(nrow(cf))) {
  nm <- rownames(cf)[i]; b <- cf[i,1]; pv <- cf[i,4]
  sig <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else
         if (pv < 0.05)  "*"   else if (pv < 0.10) "."  else " (no signif.)"

  if (nm == "(Intercept)") {
    cat(sprintf("  • BASE: En condiciones neutras se esperan %.2f goles.\n", b))
  } else {
    dir <- if (b > 0) "AUMENTA" else "DISMINUYE"
    desc <- switch(nm,
      es_local      = "Jugar como LOCAL",
      temperatura   = "Cada grado más de temperatura",
      humedad       = "Cada punto más de humedad",
      dias_descanso = "Cada día extra de descanso",
      tiros_puerta  = "Cada tiro a puerta adicional",
      posesion      = "Cada punto más de posesión",
      lesionados    = "Cada jugador lesionado",
      suspendidos   = "Cada jugador suspendido",
      es_oficial    = "Jugar partido oficial",
      nm)
    cat(sprintf("  • %s %s los goles en %.4f %s\n", desc, dir, abs(b), sig))
  }
}
r2 <- summary(modelo_reg)$r.squared
cat(sprintf("\n  R² = %.4f → explica el %.1f%% de la variabilidad en goles.\n\n",
            r2, r2*100))


# ─────────────────────────────────────────────────────────────────
# BLOQUE 8 — CÁLCULO DE GOLES ESPERADOS (LAMBDA DE POISSON)
# ─────────────────────────────────────────────────────────────────
# λ (lambda) = tasa media de goles esperados por equipo.
# Un proceso de Poisson modela eventos raros en tiempo fijo
# (goles en 90 minutos), donde λ es el parámetro central.
#
# Se construye como media ponderada de:
#   • Rendimiento como local/visitante  (50%)
#   • Rendimiento en clima similar      (15%)
#   • Rendimiento en temperatura similar(10%)
#   • Historial H2H                     (15%)
#   • Rendimiento en partidos oficiales (10%)
#
# Luego se multiplica por factores de ajuste:
#   f_clima, f_temp, f_hum, f_tipo, f_forma, f_defensa_rival,
#   f_fatiga, f_bajas, bonus_localía

cat("══════════════════════════════════════════════════════════════\n")
cat("  CÁLCULO DE GOLES ESPERADOS — PARÁMETROS LAMBDA DE POISSON\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ── Factores de ajuste externos ───────────────────────────
f_clima <- switch(tolower(CLIMA),
  "soleado"   = 1.00,
  "nublado"   = 0.97,
  "lluvia"    = 0.88,
  "frio"      = 0.92,
  "caluroso"  = 0.93,
  1.00)

f_temp <- if      (TEMPERATURA < 0  | TEMPERATURA > 38) 0.88
           else if (TEMPERATURA < 5  | TEMPERATURA > 35) 0.92
           else if (TEMPERATURA < 10 | TEMPERATURA > 30) 0.96
           else 1.00

f_hum  <- if      (HUMEDAD > 85) 0.91
           else if (HUMEDAD > 75) 0.95
           else if (HUMEDAD > 65) 0.98
           else 1.00

f_tipo <- if (tolower(TIPO_PARTIDO) == "amistoso") 0.90 else 1.00

# ── Lambda LOCAL ──────────────────────────────────────────
base_L   <- 0.50 * sL$gf_local   +
            0.15 * sL$gf_clima   +
            0.10 * sL$gf_temp    +
            0.15 * h2h_gf_L      +
            0.10 * sL$gf_oficial

# Factor forma: escala 0.82 a 1.18 según puntos obtenidos
f_forma_L  <- 0.82 + 0.36 * sL$forma_pct

# Factor nivel defensivo del rival (si el rival es muy defensivo, baja el lambda)
# Si nivel_defensivo del rival es alto (buena defensa), reduce ataque propio
f_def_rival_L <- 1 - (sV$nivel_defensivo - 0.50) * 0.35

# Bonus por jugar en casa (históricamente +0.20-0.25 goles en local)
bonus_local <- 0.22

lambda_local <- max(0.25,
  (base_L + bonus_local) *
  f_forma_L * f_def_rival_L *
  f_clima * f_temp * f_hum * f_tipo *
  sL$fatiga * sL$factor_bajas)

# ── Lambda VISITANTE ──────────────────────────────────────
base_V   <- 0.50 * sV$gf_visit   +
            0.15 * sV$gf_clima   +
            0.10 * sV$gf_temp    +
            0.15 * h2h_gf_V      +
            0.10 * sV$gf_oficial

f_forma_V     <- 0.82 + 0.36 * sV$forma_pct
f_def_rival_V <- 1 - (sL$nivel_defensivo - 0.50) * 0.35

lambda_visitante <- max(0.25,
  base_V *
  f_forma_V * f_def_rival_V *
  f_clima * f_temp * f_hum * f_tipo *
  sV$fatiga * sV$factor_bajas)

# Mostrar lambdas y factores
cat(sprintf("  λ LOCAL     [%-20s]: %.4f goles esperados\n",
            EQUIPO_LOCAL, lambda_local))
cat(sprintf("  λ VISITANTE [%-20s]: %.4f goles esperados\n",
            EQUIPO_VISITANTE, lambda_visitante))
cat(sprintf("  Total goles esperados en el partido : %.4f\n\n",
            lambda_local + lambda_visitante))

cat("  Factores de ajuste:\n")
cat(sprintf("    Clima %-12s : %.3f\n", CLIMA,       f_clima))
cat(sprintf("    Temperatura %d°C  : %.3f\n", round(TEMPERATURA), f_temp))
cat(sprintf("    Humedad %d%%       : %.3f\n", round(HUMEDAD),     f_hum))
cat(sprintf("    Tipo partido      : %.3f  (%s)\n", f_tipo, TIPO_PARTIDO))
cat(sprintf("    Forma local       : %.3f\n", f_forma_L))
cat(sprintf("    Forma visitante   : %.3f\n", f_forma_V))
cat(sprintf("    Def. rival local  : %.3f\n", f_def_rival_L))
cat(sprintf("    Def. rival visit. : %.3f\n", f_def_rival_V))
cat(sprintf("    Fatiga local      : %.3f  (%dd descanso)\n",
            sL$fatiga, DESCANSO_LOCAL))
cat(sprintf("    Fatiga visitante  : %.3f  (%dd descanso)\n",
            sV$fatiga, DESCANSO_VISITANTE))
cat(sprintf("    Bajas local       : %.3f  (%d lesion+suspend)\n",
            sL$factor_bajas, round(dL$lesionados[1] + dL$suspendidos[1])))
cat(sprintf("    Bajas visitante   : %.3f  (%d lesion+suspend)\n",
            sV$factor_bajas, round(dV$lesionados[1] + dV$suspendidos[1])))
cat(sprintf("    Bonus localía     : +%.3f\n\n", bonus_local))


# ─────────────────────────────────────────────────────────────────
# BLOQUE 9 — SIMULACIÓN MONTE CARLO (DISTRIBUCIÓN DE POISSON)
# ─────────────────────────────────────────────────────────────────
# Se simulan N_SIMULACIONES partidos asumiendo independencia:
#   Goles local     ~ Poisson(λ_local)
#   Goles visitante ~ Poisson(λ_visitante)
#
# rpois(n, λ) genera n realizaciones de la distribución Poisson.
# La frecuencia de cada marcador estimada en N=10,000 simulaciones
# converge a su probabilidad real (Ley de los Grandes Números).

cat("══════════════════════════════════════════════════════════════\n")
cat(sprintf("  SIMULACIÓN MONTE CARLO — %s PARTIDOS\n",
            format(N_SIMULACIONES, big.mark=",")))
cat("══════════════════════════════════════════════════════════════\n\n")

set.seed(42)   # semilla para reproducibilidad
sim_L <- rpois(N_SIMULACIONES, lambda_local)
sim_V <- rpois(N_SIMULACIONES, lambda_visitante)

# Tabla de marcadores con probabilidades
marc_vec  <- paste(sim_L, sim_V, sep="-")
tabla_sim <- as.data.frame(table(marc_vec), stringsAsFactors = FALSE)
names(tabla_sim) <- c("Marcador","Frecuencia")
tabla_sim$Probabilidad <- tabla_sim$Frecuencia / N_SIMULACIONES
tabla_sim$Porcentaje   <- round(tabla_sim$Probabilidad * 100, 2)
tabla_sim <- tabla_sim[order(tabla_sim$Probabilidad, decreasing = TRUE), ]
rownames(tabla_sim) <- NULL


# ─────────────────────────────────────────────────────────────────
# BLOQUE 10 — DISTRIBUCIÓN CONJUNTA ANALÍTICA (PROBABILIDAD MULTIVARIADA)
# ─────────────────────────────────────────────────────────────────
# Se calcula la probabilidad exacta de cada marcador (i-j) usando:
#   P(X=i, Y=j) = [e^(-λL) · λL^i / i!] × [e^(-λV) · λV^j / j!]
#
# La matriz resultante (filas=goles local, columnas=goles visitante)
# permite calcular exactamente todas las métricas de resultado.

cat("══════════════════════════════════════════════════════════════\n")
cat("  DISTRIBUCIÓN CONJUNTA ANALÍTICA\n")
cat("══════════════════════════════════════════════════════════════\n\n")

MAX_G <- 8   # máximo goles considerado por equipo

prob_mat <- matrix(0, MAX_G+1, MAX_G+1, dimnames=list(0:MAX_G, 0:MAX_G))
for (i in 0:MAX_G)
  for (j in 0:MAX_G)
    prob_mat[i+1, j+1] <- dpois(i, lambda_local) * dpois(j, lambda_visitante)

# Probabilidades de resultado (suma de celdas correspondientes)
p_vic_L <- sum(prob_mat[lower.tri(prob_mat, diag=FALSE)])  # i > j
p_emp   <- sum(diag(prob_mat))                             # i = j
p_vic_V <- sum(prob_mat[upper.tri(prob_mat, diag=FALSE)])  # i < j

# Over / Under 2.5
p_over25  <- 0; p_under25 <- 0
for (i in 0:MAX_G) for (j in 0:MAX_G) {
  p <- prob_mat[i+1,j+1]
  if ((i+j) > 2.5) p_over25 <- p_over25 + p else p_under25 <- p_under25 + p
}

# BTTS: ambos equipos anotan
p_btts <- 1 - dpois(0,lambda_local) - dpois(0,lambda_visitante) +
              dpois(0,lambda_local)*dpois(0,lambda_visitante)

# Over / Under 1.5 y 3.5
p_over15  <- 0; p_over35 <- 0
for (i in 0:MAX_G) for (j in 0:MAX_G) {
  p <- prob_mat[i+1,j+1]
  if ((i+j) > 1.5) p_over15 <- p_over15 + p
  if ((i+j) > 3.5) p_over35 <- p_over35 + p
}
p_under15 <- 1-p_over15; p_under35 <- 1-p_over35

cat("  PROBABILIDADES DE RESULTADO:\n")
cat("  ──────────────────────────────────────────────────\n")
cat(sprintf("  Victoria %-24s : %5.1f%%\n", EQUIPO_LOCAL,     p_vic_L*100))
cat(sprintf("  Empate                                   : %5.1f%%\n", p_emp*100))
cat(sprintf("  Victoria %-24s : %5.1f%%\n", EQUIPO_VISITANTE, p_vic_V*100))
cat("  ──────────────────────────────────────────────────\n")
cat(sprintf("  Over  1.5 goles                          : %5.1f%%\n", p_over15*100))
cat(sprintf("  Under 1.5 goles                          : %5.1f%%\n", p_under15*100))
cat(sprintf("  Over  2.5 goles                          : %5.1f%%\n", p_over25*100))
cat(sprintf("  Under 2.5 goles                          : %5.1f%%\n", p_under25*100))
cat(sprintf("  Over  3.5 goles                          : %5.1f%%\n", p_over35*100))
cat(sprintf("  Under 3.5 goles                          : %5.1f%%\n", p_under35*100))
cat(sprintf("  Ambos equipos anotan (BTTS)              : %5.1f%%\n", p_btts*100))
cat("  ──────────────────────────────────────────────────\n\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 11 — ANÁLISIS DE CORRELACIONES
# ─────────────────────────────────────────────────────────────────
# Se mide la asociación lineal (Pearson) entre todas las variables
# numéricas del dataset. Correlaciones importantes:
#   • goles_favor ↔ tiros_puerta  → más tiros = más goles?
#   • goles_favor ↔ posesion      → más posesión = más goles?
#   • goles_favor ↔ es_local      → efecto localía
#   • goles_favor ↔ temperatura   → efecto climático
#   • goles_favor ↔ lesionados    → efecto de bajas

cat("══════════════════════════════════════════════════════════════\n")
cat("  ANÁLISIS DE CORRELACIONES\n")
cat("══════════════════════════════════════════════════════════════\n\n")

vars_cor  <- c("goles_favor","goles_contra","es_local","dif_goles","puntos",
               "temperatura","humedad","dias_descanso","tiros_puerta",
               "posesion","lesionados","suspendidos","es_oficial")
vars_cor  <- vars_cor[vars_cor %in% names(datos)]
dat_cor   <- datos[, vars_cor, drop=FALSE]
dat_cor   <- dat_cor[complete.cases(dat_cor), ]

# Solo columnas con varianza > 0
vars_cor  <- vars_cor[sapply(vars_cor, function(v)
  is.numeric(dat_cor[[v]]) && var(dat_cor[[v]], na.rm=TRUE) > 1e-10)]
dat_cor   <- dat_cor[, vars_cor, drop=FALSE]

mat_cor   <- NULL
if (ncol(dat_cor) >= 2) {
  mat_cor <- cor(dat_cor, use="complete.obs")
  cat("  Matriz de correlaciones (Pearson):\n\n")
  print(round(mat_cor, 3))

  if ("goles_favor" %in% colnames(mat_cor)) {
    cat("\n  Correlaciones con 'goles_favor' (ordenadas por magnitud):\n")
    cg <- sort(mat_cor["goles_favor",], decreasing=TRUE)
    cg <- cg[names(cg)!="goles_favor"]
    for (nm in names(cg)) {
      bar <- paste(rep(if(cg[nm]>0)"+" else "-",
                       max(1,round(abs(cg[nm])*10))), collapse="")
      tipo <- if(abs(cg[nm])>0.5) "FUERTE" else if(abs(cg[nm])>0.25) "MODERADA" else "DÉBIL"
      cat(sprintf("    %-20s r=%+.3f  [%s] %s\n", nm, cg[nm], tipo, bar))
    }
  }
}
cat("\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 12 — GRÁFICAS EN R STUDIO
# ─────────────────────────────────────────────────────────────────
# Se generan 13 gráficas directamente en el panel Plots de RStudio.
# Cada ventana nueva se abre con dev.new().

cat("══════════════════════════════════════════════════════════════\n")
cat("  GENERANDO GRÁFICAS (revise el panel Plots de RStudio)...\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Paleta de colores
CL  <- "#1B4F72"   # azul oscuro  → local
CV  <- "#922B21"   # rojo oscuro  → visitante
CV2 <- "#1ABC9C"   # verde        → victoria
CE  <- "#F39C12"   # naranja      → empate
CD  <- "#E74C3C"   # rojo         → derrota

col_res_vec <- function(r) ifelse(r=="victoria",CV2,ifelse(r=="empate",CE,CD))

# ── GRÁFICA 1 & 2: Evolución de goles por partido ────────
dev.new(width=14, height=6)
par(mfrow=c(1,2), mar=c(5,4,4,2))

graficar_evol <- function(df, nombre, ca, cb) {
  n   <- nrow(df)
  idx <- seq_len(n)
  ym  <- max(c(df$goles_favor, df$goles_contra), na.rm=TRUE) + 1
  plot(idx, df$goles_favor, type="b", pch=19, lwd=2, col=ca,
       ylim=c(0,ym), xaxt="n",
       xlab="Partido (1=más reciente)", ylab="Goles",
       main=paste("Evolución de goles\n", nombre))
  axis(1, at=idx, labels=paste0("P",idx))
  lines(idx, df$goles_contra, type="b", pch=17, lwd=2, col=cb, lty=2)
  abline(h=mean(df$goles_favor),  col=ca, lty=3)
  abline(h=mean(df$goles_contra), col=cb, lty=3)
  legend("topright", c("Goles favor","Goles contra"),
         col=c(ca,cb), lwd=2, pch=c(19,17), lty=c(1,2),
         bg="white", cex=0.85)
  grid(col="gray90")
}
graficar_evol(dL, EQUIPO_LOCAL,     CL, CV)
graficar_evol(dV, EQUIPO_VISITANTE, CV, CL)

# ── GRÁFICA 3 & 4: Forma reciente (barras de puntos) ─────
dev.new(width=14, height=6)
par(mfrow=c(1,2), mar=c(5,4,4,2))

graficar_forma <- function(df, nombre) {
  n <- nrow(df)
  barplot(df$puntos, col=col_res_vec(df$resultado), border="white",
          names.arg=paste0("P",seq_len(n)), ylim=c(0,3.8),
          main=paste("Forma reciente\n", nombre),
          xlab="Partido (1=más reciente)", ylab="Puntos")
  legend("topright",
    c(sprintf("Victoria ×%d",sum(df$resultado=="victoria")),
      sprintf("Empate   ×%d",sum(df$resultado=="empate")),
      sprintf("Derrota  ×%d",sum(df$resultado=="derrota"))),
    fill=c(CV2,CE,CD), border=NA, bg="white", cex=0.85)
  grid(nx=NA, ny=NULL, col="gray90")
}
graficar_forma(dL, EQUIPO_LOCAL)
graficar_forma(dV, EQUIPO_VISITANTE)

# ── GRÁFICA 5: Dispersión goles anotados vs recibidos ────
dev.new(width=9, height=7)
par(mar=c(5,4,4,2))
dd <- rbind(
  data.frame(eq=EQUIPO_LOCAL,     gf=dL$goles_favor, gc=dL$goles_contra, cl=CL),
  data.frame(eq=EQUIPO_VISITANTE, gf=dV$goles_favor, gc=dV$goles_contra, cl=CV))
plot(dd$gf, dd$gc, col=dd$cl, pch=19, cex=1.7,
     xlab="Goles anotados", ylab="Goles recibidos",
     main="Goles anotados vs. Goles recibidos\n(cada punto = un partido)")
tryCatch({
  d1 <- dd[dd$eq==EQUIPO_LOCAL,];     if(var(d1$gf)>0) abline(lm(gc~gf,d1),col=CL,lwd=2,lty=2)
  d2 <- dd[dd$eq==EQUIPO_VISITANTE,]; if(var(d2$gf)>0) abline(lm(gc~gf,d2),col=CV,lwd=2,lty=2)
}, error=function(e) NULL)
legend("topright", c(EQUIPO_LOCAL,EQUIPO_VISITANTE), col=c(CL,CV),
       pch=19, lty=2, lwd=2, bg="white")
grid(col="gray90")

# ── GRÁFICA 6 & 7: Tiros a puerta y posesión ─────────────
dev.new(width=14, height=6)
par(mfrow=c(1,2), mar=c(5,4,4,2))

# Tiros a puerta
all_tp <- c(dL$tiros_puerta, dV$tiros_puerta)
boxplot(list(Local=dL$tiros_puerta, Visitante=dV$tiros_puerta),
        col=c(CL,CV), border="gray30", main="Tiros a puerta",
        ylab="Tiros a puerta", names=c(EQUIPO_LOCAL,EQUIPO_VISITANTE))
grid(nx=NA, ny=NULL)

# Posesión
boxplot(list(Local=dL$posesion, Visitante=dV$posesion),
        col=c(CL,CV), border="gray30", main="Posesión promedio (%)",
        ylab="Posesión (%)", names=c(EQUIPO_LOCAL,EQUIPO_VISITANTE))
abline(h=50, lty=2, col="red")
grid(nx=NA, ny=NULL)

# ── GRÁFICA 8 & 9: Histogramas goles simulados ───────────
dev.new(width=14, height=6)
par(mfrow=c(1,2), mar=c(5,4,4,2))

graficar_hist_sim <- function(sim, lam, nombre, col_e) {
  mx  <- min(max(sim)+0.5, 10.5)
  brk <- seq(-0.5, max(sim)+0.5, 1)
  hist(sim, breaks=brk, col=col_e, border="white", freq=FALSE,
       xlim=c(-0.5, mx), xlab="Goles", ylab="Densidad",
       main=paste("Distribución de goles simulados\n", nombre))
  pts <- 0:floor(mx)
  points(pts, dpois(pts,lam), type="b", pch=19, col="white", lwd=2)
  abline(v=lam, col="yellow", lwd=2, lty=2)
  legend("topright", c("Simulación","Teórico Poisson",
                        sprintf("λ = %.2f", lam)),
         col=c(col_e,"white","yellow"), pch=c(15,19,NA),
         lty=c(NA,1,2), lwd=2, bg="gray20", text.col="white", cex=0.8)
}
graficar_hist_sim(sim_L, lambda_local,     EQUIPO_LOCAL,     CL)
graficar_hist_sim(sim_V, lambda_visitante, EQUIPO_VISITANTE, CV)

# ── GRÁFICA 10: Barras Top 10 marcadores ─────────────────
dev.new(width=11, height=7)
par(mar=c(5,5,4,2))
top10 <- head(tabla_sim, 10)
grad  <- colorRampPalette(c(CL,"#5D6D7E",CV))(nrow(top10))
bp10  <- barplot(top10$Porcentaje, names.arg=top10$Marcador,
                 col=grad, border="white", ylim=c(0,max(top10$Porcentaje)*1.30),
                 main=paste("Top 10 marcadores más probables\n",
                            EQUIPO_LOCAL,"vs",EQUIPO_VISITANTE),
                 xlab="Marcador", ylab="Probabilidad (%)")
text(bp10, top10$Porcentaje + max(top10$Porcentaje)*0.04,
     paste0(top10$Porcentaje,"%"), cex=0.9, font=2)
grid(nx=NA, ny=NULL, col="gray85")

# ── GRÁFICA 11: Mapa de calor de probabilidades ───────────
MAX_HM <- min(5, MAX_G)
hm_m   <- prob_mat[1:(MAX_HM+1), 1:(MAX_HM+1)]
hm_df  <- reshape2::melt(hm_m)
names(hm_df) <- c("GL","GV","P")
hm_df$GL  <- as.integer(hm_df$GL)  - 1L
hm_df$GV  <- as.integer(hm_df$GV)  - 1L
hm_df$Lbl <- paste0(round(hm_df$P*100, 1), "%")

print(ggplot(hm_df, aes(x=factor(GV), y=factor(GL), fill=P)) +
  geom_tile(color="white", linewidth=0.7) +
  geom_text(aes(label=Lbl), color="white", size=3.8, fontface="bold") +
  scale_fill_gradient2(low="#154360", mid="#2E86C1", high="#E74C3C",
                       midpoint=median(hm_df$P), name="P") +
  labs(title=paste("Mapa de calor — Probabilidad de marcadores\n",
                   EQUIPO_LOCAL,"(local) vs",EQUIPO_VISITANTE,"(visitante)"),
       x=paste("Goles", EQUIPO_VISITANTE),
       y=paste("Goles", EQUIPO_LOCAL)) +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        panel.grid=element_blank()))

# ── GRÁFICA 12: Probabilidades de resultado ───────────────
dev.new(width=9, height=7)
par(mar=c(5,4,4,2))
pres  <- c(p_vic_L, p_emp, p_vic_V)*100
nres  <- c(paste("Victoria\n",EQUIPO_LOCAL), "Empate",
            paste("Victoria\n",EQUIPO_VISITANTE))
bp12  <- barplot(pres, names.arg=nres, col=c(CL,CE,CV), border="white",
                 ylim=c(0,max(pres)*1.30),
                 main=paste("Probabilidades de resultado\n",
                            EQUIPO_LOCAL,"vs",EQUIPO_VISITANTE),
                 ylab="Probabilidad (%)")
text(bp12, pres+max(pres)*0.05, paste0(round(pres,1),"%"), cex=1.2, font=2)
grid(nx=NA, ny=NULL, col="gray85")

# ── GRÁFICA 13: Correlaciones ─────────────────────────────
if (!is.null(mat_cor) && nrow(mat_cor) >= 2) {
  dev.new(width=9, height=8)
  corrplot::corrplot(mat_cor, method="color", type="upper",
                     tl.col="black", tl.srt=45, tl.cex=0.80,
                     addCoef.col="black", number.cex=0.65,
                     col=colorRampPalette(c(CV,"white",CL))(200),
                     title="Matriz de Correlaciones — Todas las variables",
                     mar=c(0,0,2,0))
}

# ── GRÁFICA 14: Diagnóstico del modelo de regresión ───────
dev.new(width=12, height=10)
par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(modelo_reg, which=c(1,2,3,5),
     sub.caption="Diagnóstico del modelo de regresión lineal")

# ── GRÁFICA 15: Comparativa estadísticas ──────────────────
dev.new(width=13, height=6)
par(mar=c(6,4,4,2))
vn   <- c("Goles\nanot.","Goles\nrecib.","Tiros\npuerta/3","Posesión\n/20",
           "Forma\n/15","Niv.of.\n×3","Niv.def.\n×3")
vL15 <- c(sL$gf, sL$gc, sL$tp/3, sL$pos/20, sL$forma_pts,
           sL$nivel_ofensivo*3, sL$nivel_defensivo*3)
vV15 <- c(sV$gf, sV$gc, sV$tp/3, sV$pos/20, sV$forma_pts,
           sV$nivel_ofensivo*3, sV$nivel_defensivo*3)
barplot(rbind(vL15,vV15), beside=TRUE, names.arg=vn,
        col=c(CL,CV), border="white",
        main=paste("Comparativa completa\n",EQUIPO_LOCAL,"vs",EQUIPO_VISITANTE),
        ylab="Valor estandarizado", cex.names=0.82)
legend("topright", c(EQUIPO_LOCAL,EQUIPO_VISITANTE),
       fill=c(CL,CV), border=NA, bg="white")
grid(nx=NA, ny=NULL, col="gray85")

cat("  15 gráficas generadas. Revise el panel 'Plots' en RStudio.\n\n")


# ─────────────────────────────────────────────────────────────────
# BLOQUE 13 — RESUMEN FINAL
# ─────────────────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                 RESUMEN FINAL DE PREDICCIÓN                 ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("  Partido  : %s  vs  %s\n", EQUIPO_LOCAL, EQUIPO_VISITANTE))
cat(sprintf("  Tipo     : %-18s  Clima: %-10s\n", TIPO_PARTIDO, CLIMA))
cat(sprintf("  Condic.  : %.0f°C | Humedad %.0f%%\n",
            TEMPERATURA, HUMEDAD))
cat(sprintf("  Descanso : %s %dd | %s %dd\n\n",
            EQUIPO_LOCAL, DESCANSO_LOCAL, EQUIPO_VISITANTE, DESCANSO_VISITANTE))

cat("  ── GOLES ESPERADOS (λ Poisson) ──────────────────────────\n")
cat(sprintf("    %-26s: %.3f goles\n", EQUIPO_LOCAL,     lambda_local))
cat(sprintf("    %-26s: %.3f goles\n", EQUIPO_VISITANTE, lambda_visitante))
cat(sprintf("    Total esperado               : %.3f goles\n\n",
            lambda_local+lambda_visitante))

cat("  ── MARCADOR MÁS PROBABLE ────────────────────────────────\n")
cat(sprintf("    >>>  %s   con   %.2f%%  <<<\n\n",
            tabla_sim$Marcador[1], tabla_sim$Porcentaje[1]))

cat("  ── TOP 10 MARCADORES MÁS PROBABLES ─────────────────────\n")
cat(sprintf("  %-10s  %-12s  %s\n","Marcador","Probabilidad","Barra"))
cat("  ──────────────────────────────────────────────────────\n")
for (i in seq_len(min(10,nrow(tabla_sim)))) {
  barra <- paste(rep("█", max(1, round(tabla_sim$Porcentaje[i]))), collapse="")
  cat(sprintf("  %-10s  %6.2f%%        %s\n",
              tabla_sim$Marcador[i], tabla_sim$Porcentaje[i], barra))
}

cat("\n  ── PROBABILIDADES DE RESULTADO ──────────────────────────\n")
cat(sprintf("  Victoria %-26s: %5.1f%%\n", EQUIPO_LOCAL,     p_vic_L*100))
cat(sprintf("  Empate                                   : %5.1f%%\n", p_emp*100))
cat(sprintf("  Victoria %-26s: %5.1f%%\n", EQUIPO_VISITANTE, p_vic_V*100))

cat("\n  ── MERCADOS ADICIONALES ─────────────────────────────────\n")
cat(sprintf("  Over  1.5 goles                          : %5.1f%%\n", p_over15*100))
cat(sprintf("  Under 1.5 goles                          : %5.1f%%\n", p_under15*100))
cat(sprintf("  Over  2.5 goles                          : %5.1f%%\n", p_over25*100))
cat(sprintf("  Under 2.5 goles                          : %5.1f%%\n", p_under25*100))
cat(sprintf("  Over  3.5 goles                          : %5.1f%%\n", p_over35*100))
cat(sprintf("  Under 3.5 goles                          : %5.1f%%\n", p_under35*100))
cat(sprintf("  Ambos anotan (BTTS)                      : %5.1f%%\n", p_btts*100))

cat("\n  ── INTERPRETACIÓN AUTOMÁTICA ────────────────────────────\n\n")

# Favorito
if (p_vic_L > p_vic_V + 0.08)
  cat(sprintf("  ▶ FAVORITO: %s (LOCAL) — ventaja de %.1f pp.\n",
              EQUIPO_LOCAL, (p_vic_L-p_vic_V)*100))
else if (p_vic_V > p_vic_L + 0.08)
  cat(sprintf("  ▶ FAVORITO: %s (VISITANTE) — ventaja de %.1f pp.\n",
              EQUIPO_VISITANTE, (p_vic_V-p_vic_L)*100))
else
  cat("  ▶ PARTIDO MUY EQUILIBRADO — sin favorito claro.\n")

# Tipo de partido por goles
tot <- lambda_local + lambda_visitante
cat(sprintf("  ▶ GOLES: %s\n",
  if      (tot > 4.0) "Partido MUY ABIERTO — alta probabilidad de goleada."
  else if (tot > 3.0) "Partido ABIERTO — varios goles esperados (over 2.5 probable)."
  else if (tot > 2.0) "Partido EQUILIBRADO — puede ir de cualquier forma."
  else if (tot > 1.5) "Partido con POCOS GOLES — defensa dominante."
  else                "Partido MUY CERRADO — empate 0-0 o 1-0 más probables."))

# Forma reciente
if (sL$forma_pct > sV$forma_pct + 0.20)
  cat(sprintf("  ▶ FORMA: %s llega mejor (%d vs %d pts últimos 5 p.).\n",
              EQUIPO_LOCAL, sL$forma_pts, sV$forma_pts))
else if (sV$forma_pct > sL$forma_pct + 0.20)
  cat(sprintf("  ▶ FORMA: %s llega mejor (%d vs %d pts últimos 5 p.).\n",
              EQUIPO_VISITANTE, sV$forma_pts, sL$forma_pts))
else
  cat(sprintf("  ▶ FORMA: Similares (%d vs %d pts en últimos 5 partidos).\n",
              sL$forma_pts, sV$forma_pts))

# Racha
cat(sprintf("  ▶ RACHA LOCAL    : %d partido(s) consecutivo(s) en '%s'.\n",
            sL$racha_n, sL$racha_tipo))
cat(sprintf("  ▶ RACHA VISITANTE: %d partido(s) consecutivo(s) en '%s'.\n",
            sV$racha_n, sV$racha_tipo))

# Clima
if (f_clima < 0.95)
  cat(sprintf("  ▶ CLIMA: El %s puede reducir el ritmo de juego (factor %.2f).\n",
              CLIMA, f_clima))

# Fatiga
if (sL$fatiga < 1)
  cat(sprintf("  ▶ FATIGA: %s con solo %dd de descanso (factor %.2f).\n",
              EQUIPO_LOCAL, DESCANSO_LOCAL, sL$fatiga))
if (sV$fatiga < 1)
  cat(sprintf("  ▶ FATIGA: %s con solo %dd de descanso (factor %.2f).\n",
              EQUIPO_VISITANTE, DESCANSO_VISITANTE, sV$fatiga))

# Bajas
bajas_L_act <- round(dL$lesionados[1] + dL$suspendidos[1])
bajas_V_act <- round(dV$lesionados[1] + dV$suspendidos[1])
if (bajas_L_act >= 3)
  cat(sprintf("  ▶ BAJAS: %s tiene %d jugadores fuera (impacto: %.2f).\n",
              EQUIPO_LOCAL, bajas_L_act, sL$factor_bajas))
if (bajas_V_act >= 3)
  cat(sprintf("  ▶ BAJAS: %s tiene %d jugadores fuera (impacto: %.2f).\n",
              EQUIPO_VISITANTE, bajas_V_act, sV$factor_bajas))

# H2H
if (hay_h2h)
  cat(sprintf("  ▶ H2H: Historial de %d enfrentamientos directo(s) en el dataset.\n",
              nrow(h2h)))

cat("\n")
cat("  ╔──────────────────────────────────────────────────────────╗\n")
cat("  ║  NOTA METODOLÓGICA                                       ║\n")
cat("  ║  Este modelo estima probabilidades con base en datos     ║\n")
cat("  ║  históricos. No predice con certeza el resultado real.   ║\n")
cat("  ║  El fútbol tiene varianza aleatoria inherente que        ║\n")
cat("  ║  ningún modelo puede eliminar completamente.             ║\n")
cat("  ╚──────────────────────────────────────────────────────────╝\n\n")

cat("  Análisis completado. Revise las 15 gráficas en RStudio.\n\n")
