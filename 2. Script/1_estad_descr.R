
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

###*** 1. Estadísticas descriptivas ***###
## se importa bases de datos creada en 2_WTI models
df<- readRDS("df.rds")
class(df)
dim(df)
colnames(df)
summary(df)

# estadísiticas descriptivas generales datos

stat.desc(df)
descriptivas <- stat.desc(df) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivas$Estadisticas <- row.names(descriptivas) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivas <- descriptivas %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivas, "descriptivas.xlsx") # se exporta a excel tabla con las estadísticas descriptivas


