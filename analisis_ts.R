# Analisis de la serie de tiempo

library(lmtest)
library(forecast)

## Funciones 
source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

Xt <- na.omit(df$co)
Xt <- ts(Xt, start = c(1997,5), frequency = 12)
t <- as.numeric(time(Xt))
plot(Xt, lwd = 2)

# Test -----

ks.test(Xt,"pnorm") # no son normales
bptest(Xt ~ t) # no son homocedasticos

lambda <- forecast::BoxCox.lambda(Xt, method = "guerrero")

plot(forecast::BoxCox(Xt, lambda = lambda), col = "steelblue", lwd = 2, xaxt = "n", ylab = "", main = "Transformación Box Cox", xlab = "")

fit <- forecast::auto.arima(Xt, lambda = lambda)
fit$fitted
TS.diag(fit$res)
salida.arima(Xt, fit)

acf(Xt) # ma -> 2
pacf(Xt) # ar -> 4

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
library(splines)
mod41 <- lm(Xt ~ bs(t)) # Ajusta por defecto un polinomio cubico
mod42 <- lm(Xt ~ bs(t, knots = c(2000, 2007.917, 2017)))
mod43 <- lm(Xt ~ bs(t, knots = c(2000, 2007.917, 2017), degree = 1))
par(mfrow = c(1,1))
plot(Xt)
lines(mod41$fitted.values ~ t, col = "red", lwd = 2)
lines(mod42$fitted.values ~ t, col = "blue", lwd = 2)
lines(mod43$fitted.values ~ t, col = "green3", lwd = 2)
