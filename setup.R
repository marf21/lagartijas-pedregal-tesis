# setup.R - Configuración del proyecto de tesis
# Ejecutar este archivo al inicio de cada sesión

# Instalar pacman si no está
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Cargar paquetes necesarios (los instala si no los tienes)
p_load(glmmTMB, MASS, performance, DHARMa, ggplot2, dplyr)

# Verificar que están cargados
cat("¡Paquetes cargados correctamente!\n")
sessionInfo()
