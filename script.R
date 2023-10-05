## Chat (a ver hasta donde llega esta wea)

# Hola
# ola, vuenoz díaz. Eskrivo pq haz ganao' un concurzo, nesecito zu numéro de targeta de credíto 



# Paquetes
library(tidyverse)
library(dplyr)
library(lubridate) # por el año
library(forecast)
# library(foreign)  No sé si va esta wea


# Carga de datos
data <- rio::import("datos_970410_231003.csv")


# Ponemos el formato de las fechas
data$`FECHA (YYMMDD)` = as.Date(
  # para los que son 00 0X 01
  ifelse(data$`FECHA (YYMMDD)` < 1000, 
         paste0("000", data$`FECHA (YYMMDD)`),
         # para los 3 que son 00 1X 01
         ifelse(data$`FECHA (YYMMDD)` < 2000, 
                paste0("00", data$`FECHA (YYMMDD)`),
                # Para los que son 0X XX 01
                ifelse(data$`FECHA (YYMMDD)` < 100000, 
                       paste0("0", data$`FECHA (YYMMDD)`),
                       as.character(data$`FECHA (YYMMDD)`)))),
  format = "%y%m%d"
)



