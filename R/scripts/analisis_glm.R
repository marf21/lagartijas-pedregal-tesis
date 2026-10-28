# ============================================================
# analisis_glm.R - Análisis de abundancia con GLM binomial negativo
# Tesis: ecología poblaciones Sceloporus torquatus y S. grammicus en remanentes del pedregal
# ============================================================

# 0. CARGAR CONFIGURACIÓN Y DATOS
# ------------------------------------------------------------
source("R/scripts/setup.R")

# 0.5. EJECUTAR ESTE SCRIPT DE UNA
# ------------------------------------------------------------
# ----> source("R/scripts/analisis_glm.R")


# 1. ANÁLISIS EXPLORATORIO
summary(datos$AVISTAM)
table(datos$SITIO, datos$HABITAT)
table(datos$ESPECIE, datos$temporada)

# 2. PALETA DE COLORES OKABE-ITO (apta para daltónicos)
colores_tesis <- c(
  "BARDA" = "#E69F00", "PEDREGAL" = "#56B4E9",
  "Secas_calidas" = "#F0E442", "Lluvias" = "#009E73", "Secas_frias" = "#0072B2"
)

# 3. GRÁFICOS
p1 <- ggplot(datos, aes(x = SITIO, y = AVISTAM, fill = HABITAT)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2),
              alpha = 0.4, size = 1.2) +
  scale_fill_manual(values = colores_tesis) +
  labs(x = "Sitio", y = "Abundancia", fill = "Hábitat",
       title = "Abundancia por sitio y hábitat") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("outputs/boxplot_sitio_habitat.png", plot = p1, width = 8, height = 6, dpi = 300)

p2 <- ggplot(datos, aes(x = ESPECIE, y = AVISTAM, fill = temporada)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2),
              alpha = 0.4, size = 1.2) +
  scale_fill_manual(values = colores_tesis) +
  labs(x = "Especie", y = "Abundancia", fill = "Temporada",
       title = "Abundancia por especie y temporada") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("outputs/boxplot_especie_temporada.png", plot = p2, width = 8, height = 6, dpi = 300)

# 4. MODELO GLM
modelo_glm <- glm.nb(AVISTAM ~ SITIO * HABITAT + temporada + ESPECIE, data = datos)
summary(modelo_glm)

# 5. DIAGNÓSTICO
simulacion <- simulateResiduals(modelo_glm)
plot(simulacion)

# 6. GUARDAR RESULTADOS
sink("outputs/resumen_glm.txt")
print(summary(modelo_glm)) #porque luego no lo escribe
sink()

cat("✅ Análisis completado. Resultados guardados en outputs/\n")
