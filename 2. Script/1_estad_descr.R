
# Estadísticas descriptivas
# Proyecto Final
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())

setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Proyecto_Final/4. Stores")

## llamado librerías de la sesión
require(pacman)

p_load(tidyverse,
       rvest,
       writexl,
       rio,
       stargazer,
       gtsummary,
       stringr,
       plotly, 
       leaflet
       )