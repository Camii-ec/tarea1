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
colnames(data)

df <- janitor::clean_names(data)
colnames(df)

# Ponemos el formato de las fechas
df$fecha_yymmdd = as.Date(
  # para los que son 00 0X 01
  ifelse(df$fecha_yymmdd < 1000, 
         paste0("000", df$fecha_yymmdd),
         # para los 3 que son 00 1X 01
         ifelse(df$fecha_yymmdd < 2000, 
                paste0("00", df$fecha_yymmdd),
                # Para los que son 0X XX 01
                ifelse(df$fecha_yymmdd < 100000, 
                       paste0("0", df$fecha_yymmdd),
                       as.character(df$fecha_yymmdd)))),
  format = "%y%m%d"
)

# Analizaremos el monoxido de carbono
df$co <- ifelse(
  df$registros_validados != "",
  # Primero los registros válidos
  df$registros_validados,
  ifelse(
    !is.na(df$registros_preliminares),
    # Segundo los registros preliminares
    df$registros_preliminares,
    # Tercero los registros no válidos
    df$registros_no_validados
  )
)

# Declaramos de donde vienen los datos
# por qué? pues porque si
df$tipo_registro = ifelse(
  df$registros_validados != "",
  # Primero los registros válidos
  "Registro válido",
  ifelse(
    !is.na(df$registros_preliminares),
    # Segundo los registros preliminares
    "Registro preliminar",
    # Tercero los registros no válidos
    "Registro no válido"
  )
)

# Declaramos los datos que usaremos en la construcción del modelo
# y los datos que se ocuparán en la validación
df$uso =ifelse(df$fecha_yymmdd < as.Date("2022-12-31"),
                 "Entrenamiento",
                 "Validación")



df$co <- as.numeric(gsub(",", ".", df$co))

df %>%
  select(fecha_yymmdd, co, tipo_registro, uso) %>%
  ggplot(aes(x = fecha_yymmdd, y = co, color = uso)) +
  geom_line() +  # color = "#9B098A"
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(color = "Destinación de los datos")




