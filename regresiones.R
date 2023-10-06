library(tidyverse)
library(forecast)
library(lmtest)

# Xt de entrenamiento -----------------------------------------------------

df_train <- df %>% 
  filter(uso == "Entrenamiento") %>% 
  select(-uso) 

xt_train <- na.omit(df_train$co)  
xt_train <- ts(xt_train, start = c(1997, 5), frequency = 12)

t_train <- as.numeric(xt_train)

plot(xt_train, lwd = 2)


# Xt de validación --------------------------------------------------------

df_test <- df %>% 
  filter(uso == "Validación") %>% 
  select(-uso) 

xt_test <- na.omit(df_test$co)  
xt_test <- ts(xt_test, start = c(2023, 1), frequency = 12)

t_test <- as.numeric(xt_test)
plot(xt_test, lwd = 2)

# Ajuste de regresión lineal ----------------------------------------------

modelito1 <- lm(xt_train ~ t_train)
summary(modelito1)

plot(xt_train)
abline(modelito1$coefficients[1],
       modelito1$coefficients[2], col = "red", lwd = 2)
# Ajusta como el pico, no se ve ni la recta de regresión

pred_lineal <- predict(modelito1, data.frame(t_train = t_test), interval = "prediction")

plot(xt_test, lwd = 2)
lines(ts(pred_lineal[,1], start = c(2023, 1), frequency = 12), 
      type = "l", col = "red", lty = 3, lwd = 2)
# Demasiado perfecto pa ser real

plot(modelito1$residuals)

shapiro.test(modelito1$residuals) # Rechaza normalidad
lmtest::bptest(modelito1) # Manda a la chucha la homocedasticidad

car::qqPlot(modelito1$residuals) # qué es esto Dio mío. El 2 es outlier
car::qqPlot(modelito1$residuals[-2]) # esta weá parece cuadrática

## MAPE
mean(abs((xt_test-ts(pred_lineal[,1], start = c(2023, 1), 
                     frequency = 12))/xt_test)) * 100

# Ajuste de regresión polinomial ------------------------------------------

modelito2 <- lm(xt_train ~ I(t_train^2))
summary(modelito2)
# Este si ajustaa, aunque no sé qué tan legal es hacerlo cuadrático en lugar de polinomial

pred_cuad <- predict(modelito2, data.frame(t_train = t_test), interval = "prediction")

plot(xt_test, lwd = 2)
lines(ts(pred_cuad[,1], start = c(2023, 1), frequency = 12), 
      type = "l", col = "red", lty = 3, lwd = 2)


shapiro.test(modelito2$residuals) # Rechaza normalidad
lmtest::bptest(modelito2) # Manda a la ctm la homocedasticidad

car::qqPlot(modelito2$residuals) # qué está pasando aquí

## MAPE
mean(abs((xt_test-ts(pred_cuad[,1], start = c(2023, 1), 
                     frequency = 12))/xt_test)) * 100


# Ajuste de regresión splines ----------------------------------------------

library(splines)
modelito_sp <- lm(xt_train ~ bs(t_train)) # cúbico no más, porque sí
summary(modelito_sp)
