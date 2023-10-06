# Analisis de la serie de tiempo

library(lmtest)
library(splines)
library(forecast)
library(tidyverse)
library(dplyr)

## Funciones 
source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

train <- df %>% 
  filter(uso == "Entrenamiento")

Xt <- na.omit(train$co)
Xt <- ts(Xt, start = c(1997,5), frequency = 12)
t <- as.numeric(time(Xt))

str(df)

train %>% 
ggplot(aes(x = fecha_yymmdd, y = co)) +
  geom_line(color = "#808080", size = 0.8) +  
  labs(title = "Variación del Monóxido de Carbono en ppm en Estación Pudahuel",
       subtitle = "Registros válidos desde mayo de 1997 a diciembre del 2022",
       caption = "Datos otorgados por Ministerio del Medio Ambiente de Chile",
       x = "Fecha", y = "Valor") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")

# Test -----

ks.test(Xt,"pnorm") # no son normales creo
bptest(Xt ~ t) # no son homocedasticos creo

lambda <- forecast::BoxCox.lambda(Xt, method = "guerrero")

fit <- forecast::auto.arima(Xt, lambda = lambda)
TS.diag(fit$res)
salida.arima(Xt, fit)
par(mfrow = c(1,1))

acf(Xt) 
pacf(Xt)

Xt_new <- fitted(fit)

# De la vilca
predicciones_validacion <- forecast(fit, newdata = t_test)

autoplot(predicciones_validacion)

summary(predicciones_validacion)

valores_predichos_validacion <- predicciones_validacion$mean


autoplot(Xt_new)+ 
  geom_line(color = "#808080", size = 0.8) +  
  labs(title = "Valores ajustados con transformación Box-Cox del\n Monóxido de Carbono en ppm en Estación Pudahuel",
       subtitle = "Registros válidos desde mayo de 1997 a diciembre del 2022",
       caption = "Datos otorgados por Ministerio del Medio Ambiente de Chile",
       x = "Fecha", y = "Valor")

ks.test(fit$residuals,"pnorm") # no son normales
bptest(fit$residuals ~ t) # son homocedasticos

MASS::boxcox(lm(Xt ~ Xt_new))

boxcox_plot <- MASS::boxcox(lm(Xt ~ Xt_new), plotit = FALSE)
residuos <- resid(lm(Xt ~ Xt_new))

plot(boxcox_plot, col = "purple", type = "l", lwd= 3,
     main = expression("Posibles valores de "*lambda*" para Transformación Box-Cox"),
     xlab = expression("Valor de "*lambda),
     ylab = "Estimación de la log-verosimilitud")

abline(h = boxcox_plot$y[which.max(boxcox_plot$y)], col = "blue", lty = 2)
abline(v = boxcox_plot$x[which.max(boxcox_plot$y)], col = "red", lty = 2)
abline(v = boxcox_plot$x[which.max(boxcox_plot$y)] - 1.96/sqrt(length(residuos)), col = "blue", lty = 2)
abline(v = boxcox_plot$x[which.max(boxcox_plot$y)] + 1.96/sqrt(length(residuos)), col = "blue", lty = 2)
text(x = 1, y = -800, labels = expression(lambda*"= 0.4"))

## 1) Regresion Lineal -----

mod1 <- lm(Xt ~ t)

plot(Xt)
abline(mod1$coefficients[1],mod1$coefficients[2], col = "red", lwd = 2)
# Malo

res1 <- mod1$residuals
plot(res1, type = "l")
abline(h = 0, lty = 2, lwd = 2)

## 2) Regresion Polinomial -----
# Ajustemos una funcion cuadratica

mod2 <- lm(Xt ~ t + I(t^2))
summary(mod2)
plot(Xt)
lines(mod2$fitted.values ~ t, col = "red", lwd = 2)

res <- mod2$residuals
plot(res ~ t, type = "l")
abline(h = 0, lty = 2, lwd = 2) 

## 3) Piecewise splines - Ajustar regresiones a trozos -----

mod41 <- lm(Xt ~ bs(t)) # Ajusta por defecto un polinomio cubico
mod42 <- lm(Xt ~ bs(t, knots = c(2000, 2007.917, 2017)))
mod43 <- lm(Xt ~ bs(t, knots = c(2000, 2007.917, 2017), degree = 1))
par(mfrow = c(1,1))
plot(Xt)
lines(mod41$fitted.values ~ t, col = "red", lwd = 2)
lines(mod42$fitted.values ~ t, col = "blue", lwd = 2)
lines(mod43$fitted.values ~ t, col = "green3", lwd = 2)



