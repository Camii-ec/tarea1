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

# Analizaremos el monoxido de carbono
data$CO = ifelse(
  data$`Registros validados` != "",
  # Primero los registros válidos
  data$`Registros validados`,
  ifelse(
    !is.na(data$`Registros preliminares`),
    # Segundo los registros preliminares
    data$`Registros preliminares`,
    # Tercero los registros no válidos
    data$`Registros no validados`
  )
)

# Declaramos de donde vienen los datos
# por qué? pues porque si
data$`Tipo Registro` = ifelse(
  data$`Registros validados` != "",
  # Primero los registros válidos
  "Registro válido",
  ifelse(
    !is.na(data$`Registros preliminares`),
    # Segundo los registros preliminares
    "Registro preliminar",
    # Tercero los registros no válidos
    "Registro no válido"
  )
)

# Declaramos los datos que usaremos en la construcción del modelo
# y los datos que se ocuparán en la validación
data$Uso =ifelse(data$FECHA < as.Date("2022-12-31"),
                 "Entrenamiento",
                 "Validación")



data$CO <- as.numeric(gsub(",", ".", data$CO))

data %>%
  select(FECHA, CO, `Tipo Registro`, Uso) %>%
  rename(
    Fecha = FECHA,
    CO = CO,
    `Tipo Registro` = `Tipo Registro`,
    Uso = Uso
  ) %>%
  ggplot(aes(x = Fecha, y = CO, color = Uso)) +
  geom_line() +  # color = "#9B098A"
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(color = "Destinación de los datos")




