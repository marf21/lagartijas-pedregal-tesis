# ============================================================
# distance_sampling.R - Análisis de densidad con Distance Sampling
# ============================================================

# 0. CARGAR PAQUETES
library(Distance)
library(ggplot2)
library(readr)

# 0.5. EJECUTAR ESTE SCRIPT DE UNA
# ------------------------------------------------------------
# ----> source("R/scripts/distance_sampling.R")
# ============================================================

# 1. CARGAR DATOS
densidad <- read_csv("data/processed/densidad_DISTANCE.csv")

# Eliminar columnas vacías (...10, ...11)
densidad <- densidad[, !grepl("^\\.\\.\\.", colnames(densidad))]
colnames(densidad)

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
# Histograma de distancias (con truncamiento visual a 20 m)
hist(densidad_reales$Distance[densidad_reales$Distance <= 20],
     breaks = 40,
     main = "Distribución de distancias (≤ 20 m)",
     xlab = "Distancia (m)", ylab = "Frecuencia",
     col = "steelblue")

# Boxplot por hábitat (solo distancias ≤ 20 m)
boxplot(Distance ~ Habitat,
        data = densidad_reales[densidad_reales$Distance <= 20, ],
        main = "Distancia por hábitat (≤ 20 m)",
        xlab = "Hábitat", ylab = "Distancia (m)",
        col = c("orange", "skyblue"))

# Boxplot por sitio (solo distancias ≤ 20 m)
boxplot(Distance ~ Sitio,
        data = densidad_reales[densidad_reales$Distance <= 20, ],
        main = "Distancia por sitio (≤ 20 m)",
        xlab = "Sitio", ylab = "Distancia (m)",
        col = c("lightgreen", "lightcoral", "lightblue"))

# 6. PREPARAR DATOS PARA DISTANCE SAMPLING
datos_ds <- data.frame(
  Region.Label = densidad_reales$Sitio,
  Sample.Label = densidad_reales$Transecto_ID,  # ← ID real (1-36)
  Effort = densidad_reales$Effort,
  distance = densidad_reales$Distance,
  detected = 1,
  Habitat = densidad_reales$Habitat
)

# Verificar
head(datos_ds)
str(datos_ds)
table(datos_ds$Region.Label)
table(datos_ds$Sample.Label)
table(datos_ds$Region.Label, datos_ds$Sample.Label)

# 7. AJUSTAR FUNCIÓN DE DETECCIÓN (half-normal, truncamiento = 12 m)
modelo_ds <- ds(datos_ds,
                truncation = 12,
                key = "hn",
                adjustment = NULL)

# Resumen del modelo
summary(modelo_ds)

# Gráfico de la función de detección
plot(modelo_ds)

# 8. MODELO CON HÁBITAT COMO COVARIABLE
modelo_ds_habitat <- ds(datos_ds,
                       truncation = 12,
                       key = "hn",
                       adjustment = NULL,
                       formula = ~ Habitat)
# Resumen modelo
summary(modelo_ds_habitat)

# Gráfico de función de detección con hábitat
plot(modelo_ds_habitat)

# GLM con interacción ESPECIE * SITIO * HABITAT
modelo_especies <- glm.nb(AVISTAM ~ ESPECIE * SITIO * HABITAT + temporada, data = datos)
summary(modelo_especies)
