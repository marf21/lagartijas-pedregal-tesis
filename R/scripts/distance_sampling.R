# ============================================================
# distance_sampling.R - Análisis de densidad con Distance Sampling
# ============================================================

# 0. CARGAR PAQUETES
library(Distance)
library(readxl)
library(ggplot2)

# 0.5. EJECUTAR ESTE SCRIPT DE UNA
# ------------------------------------------------------------
# ----> source("R/scripts/distance_sampling.R")
# ============================================================

# 1. CARGAR DATOS
densidad <- read_excel("data/raw/tabla_DENSIDAD.xlsx")

# 2. LIMPIAR DATOS
densidad$Distance[densidad$Distance == "NA"] <- NA
densidad$Especie[densidad$Especie == "NA"] <- NA
densidad$Avistamiento[densidad$Avistamiento == "NA"] <- NA
densidad$Distance <- as.numeric(densidad$Distance)
densidad$Avistamiento <- as.numeric(densidad$Avistamiento)

# 3. FILTRAR SOLO DETECCIONES REALES
densidad_reales <- densidad[!is.na(densidad$Distance) &
                              !is.na(densidad$Especie) &
                              !is.na(densidad$Avistamiento), ]

# 4. VERIFICACIÓN
cat("✅ Detecciones reales:", nrow(densidad_reales), "\n")
cat("✅ Especies:", paste(unique(densidad_reales$Especie), collapse = ", "), "\n")
cat("✅ Hábitats:", paste(unique(densidad_reales$Habitat), collapse = ", "), "\n")
cat("✅ Sitios:", paste(unique(densidad_reales$Sitio), collapse = ", "), "\n")

# 5. EXPLORAR DISTANCIAS
summary(densidad_reales$Distance)
hist(densidad_reales$Distance, breaks = 30,
     main = "Distribución de distancias perpendiculares",
     xlab = "Distancia (m)", ylab = "Frecuencia",
     col = "steelblue")
boxplot(Distance ~ Habitat, data = densidad_reales,
        main = "Distancia por hábitat",
        xlab = "Hábitat", ylab = "Distancia (m)",
        col = c("orange", "skyblue"))
