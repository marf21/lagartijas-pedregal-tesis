# ============================================================
# setup.R - Configuración del proyecto de tesis
# Ejecutar al inicio de cada sesión de RStudio
# ----> source("R/scripts/setup.R")
# ============================================================

# 1. CARGAR PAQUETES
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(
  MASS, glmmTMB, performance, DHARMa,
  ggplot2, dplyr, tidyr, readxl, writexl
)

# 2. CARGAR DATOS
datos <- read.csv("data/processed/lagartijas-datosagregados.csv", header = TRUE)

# 3. PREPARAR VARIABLES
# Convertir categóricas a factores
datos$ESPECIE <- as.factor(datos$ESPECIE)
datos$SITIO   <- as.factor(datos$SITIO)
datos$HABITAT <- as.factor(datos$HABITAT)

# 4. CONVERTIR MES (abreviaturas) A NÚMERO
datos$MES_num <- ifelse(datos$MES == "ENE", 1,
                        ifelse(datos$MES == "FEB", 2,
                               ifelse(datos$MES == "MAR", 3,
                                      ifelse(datos$MES == "ABR", 4,
                                             ifelse(datos$MES == "MAY", 5,
                                                    ifelse(datos$MES == "JUN" | datos$MES == "JUNI", 6,
                                                           ifelse(datos$MES == "JUL", 7,
                                                                  ifelse(datos$MES == "AGO", 8,
                                                                         ifelse(datos$MES == "SEP", 9,
                                                                                ifelse(datos$MES == "OCT", 10,
                                                                                       ifelse(datos$MES == "NOV", 11,
                                                                                              ifelse(datos$MES == "DIC", 12, NA))))))))))))

datos$MES_num <- as.numeric(datos$MES_num)

# 5. CREAR MES_factor CON ORDEN CRONOLÓGICO
datos$MES_factor <- factor(datos$MES_num,
                           levels = 1:12,
                           labels = c("Enero", "Febrero", "Marzo", "Abril",
                                      "Mayo", "Junio", "Julio", "Agosto",
                                      "Septiembre", "Octubre", "Noviembre", "Diciembre"))

# 6. CREAR TEMPORADA (3 categorías del Pedregal de San Ángel)
datos$temporada <- ifelse(datos$MES_num %in% c(6, 7, 8, 9), "Lluvias",
                          ifelse(datos$MES_num %in% c(10, 11, 12, 1), "Secas_frias",
                                 ifelse(datos$MES_num %in% c(2, 3, 4, 5), "Secas_calidas", NA)))

# Orden cronológico: Secas_calidas → Lluvias → Secas_frias
datos$temporada <- factor(datos$temporada,
                          levels = c("Secas_calidas", "Lluvias", "Secas_frias"))

# 7. VERIFICACIÓN FINAL
cat("✅ Paquetes cargados correctamente\n")
cat("✅ Datos cargados:", nrow(datos), "filas y", ncol(datos), "columnas\n")
cat("✅ Nombres de columnas:", paste(colnames(datos), collapse = ", "), "\n")
cat("✅ Niveles de temporada:", paste(levels(datos$temporada), collapse = " → "), "\n")
cat("✅ Distribución de temporada:\n")
print(table(datos$temporada))
