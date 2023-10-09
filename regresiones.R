library(tidyverse)
library(forecast)
library(lmtest)

# Xt de entrenamiento -----------------------------------------------------

# Rellenar datos faltantes
junio <- na.omit(subset(df, format(fecha_yymmdd, "%m") == "06")$co)
julio <- na.omit(subset(df, format(fecha_yymmdd, "%m") == "07")$co)

mean(junio)
mean(julio)


df_train <- df %>%
  filter(uso == "Entrenamiento") %>% 
  select(-uso) 

df_train <- df_train[df_train$fecha_yymmdd >= as.Date("1997-12-31"),]

xt_train <- ts(df_train$co, start = c(1997, 5), frequency = 12)
xt_train[is.na(xt_train)] <- c(mean(junio), mean(julio))

sum(is.na(xt_train))
sum(is.na(df$co))

t_train <- c(time(xt_train))

plot(xt_train, lwd = 2)


# Xt de validación --------------------------------------------------------

df_test <- df %>% 
  filter(uso == "Validación") %>% 
  select(-uso) 

xt_test <- ts(df_test$co, start = c(2023, 1), end = c(2023, 9), frequency = 12)
na.omit(xt_test)

t_test <- c(time(xt_test))
plot(xt_test, lwd = 2)


# Datazos previos al ajuste -----------------------------------------------

# Estacionalidad
ggAcf(xt_train) # Cada 12 lags se repite la weá
acf(xt_train)

per <- LSTS::periodogram(xt_train)
plot(per$periodogram ~ per$lambda, type = "l", lwd = 2)

which(per$periodogram  == max(per$periodogram))
d <- (2*pi)/per$lambda[26] 
abline(v = 2*pi/d, col = "red", lty = 2, lwd = 2) #¿en volá regresión con dummies pa arreglar esto? o diferenciación


# Regresión lineal con dummies --------------------------------------------

D <- rep(1:12, 25)

train <- data.frame(x = xt_train, t = t_train, D = as.factor(D))
test <- data.frame(x = xt_test, t = t_test, D = as.factor(1:9))

mod <- lm(x ~ t*(D), data = train) # Interacciones significativas
summary(mod)

plot(xt_train)
lines(mod$fitted.values ~ t_train, col = "red")

pred_lineal <- predict(mod, test, interval = "prediction")
plot(xt_test, lwd = 2)
lines(ts(pred_lineal[,1], start = c(2023, 1), frequency = 12), 
      col = "red", lty = 3, lwd = 2)
# Predice como el hoyo ah

## MAPE
mean(abs((xt_test-ts(pred_lineal[,1], start = c(2023, 1), 
                     frequency = 12))/xt_test)) * 100 # YA NO ES COMO EL HOYO


plot(mod$residuals, type = "l") # No pareciera haber tendencia
shapiro.test(mod$residuals) # Rechaza normalidad
lmtest::bptest(mod) # Manda a la chucha la homocedasticidad

acf(mod$residuals)
pacf(mod$residuals)

ggAcf(mod$residuals)
ggPacf(mod$residuals)

a <- MASS::boxcox(mod)
lambda <- a$x[which.max(a$y)]

xt_train2.0 <- (xt_train^lambda - 1)/lambda
xt_test2.0 <- (xt_test^lambda - 1)/lambda

## Ajuste 2.0 con la transformación de box-cox ---

train2.0 <- data.frame(x = xt_train2.0, t = t_train, D = as.factor(D))
test2.0 <- data.frame(x = xt_test2.0, t = t_test, D = as.factor(1:9))

mod <- lm(x ~ t*(D), data = train2.0) # Interacciones significativas
summary(mod)

plot(xt_train2.0)
lines(mod$fitted.values ~ t_train, col = "red")

pred <- forecast(mod, newdata = test2.0)
pred_box <- (pred$mean*lambda+1)^(1/lambda)

plot(xt_test, lwd = 2)
lines(ts(pred_box, start = c(2023, 1), frequency = 12), 
      col = "red", lty = 3, lwd = 2)
# YA NO ES COMO EL HOYO X2

plot(mod$residuals, type = "l") # No pareciera haber tendencia
shapiro.test(mod$residuals) # Rechaza normalidad
lmtest::bptest(mod) # ES HOMOCEDÁSTICO

acf(mod$residuals)
pacf(mod$residuals)

MLmetrics::MAPE(y_pred = pred_box,
                y_true = xt_test)*100 # TA MUY BONITA LA WEÁ, YA NO VALE PICO

sqrt(MLmetrics::MSE(y_pred = pred_box,
                    y_true = xt_test)) #RMSE

MLmetrics::MAE(y_pred = pred_box,
               y_true = xt_test) #MAE


# Ajuste de regresión lineal ----------------------------------------------

modelito1 <- lm(xt_train ~ t_train)
summary(modelito1)

plot(xt_train)
abline(modelito1$coefficients[1],
       modelito1$coefficients[2], col = "red", lwd = 2)
# Ajusta como el pico

pred_lineal <- predict(modelito1, data.frame(t_train = t_test), interval = "prediction")

plot(xt_test, lwd = 2)
lines(ts(pred_lineal[,1], start = c(2023, 1), frequency = 12), 
      type = "l", col = "red", lty = 3, lwd = 2)
# Pésima la weá

plot(modelito1$residuals)

shapiro.test(modelito1$residuals) # Rechaza normalidad
lmtest::bptest(modelito1) # Manda a la chucha la homocedasticidad

car::qqPlot(modelito1$residuals) # ¿cuadrático en volá?

## MAPE
mean(abs((xt_test-ts(pred_lineal[,1], start = c(2023, 1), 
                     frequency = 12))/xt_test)) * 100 # txiuuu

# Ajuste de regresión polinomial ------------------------------------------

modelito2 <- lm(xt_train ~ t_train + I(t_train^2))
summary(modelito2)
# R^2 ajustado es como el hoyo

pred_cuad <- predict(modelito2, data.frame(t_train = t_test), interval = "prediction")

plot(xt_test, lwd = 2)
lines(ts(pred_cuad[,1], start = c(2023, 1), frequency = 12), 
      type = "l", col = "red", lty = 3, lwd = 2)


shapiro.test(modelito2$residuals) # Rechaza normalidad
lmtest::bptest(modelito2) # Manda a la ctm la homocedasticidad

car::qqPlot(modelito2$residuals) # qué está pasando aquí

## MAPE
mean(abs((xt_test-ts(pred_cuad[,1], start = c(2023, 1), 
                     frequency = 12))/xt_test)) * 100 #Mape sigue siendo como el hoyo

par(mfrow = c(1,2))

acf(modelito2$residuals)
pacf(modelito2$residuals)

# Ajuste de regresión splines ----------------------------------------------

library(splines)
modelito_sp <- lm(xt_train ~ bs(t_train)) # cúbico no más, porque sí
summary(modelito_sp)

pred_cuad <- predict(modelito_sp, data.frame(t_train = t_test), interval = "prediction")

plot(xt_test, lwd = 2)
lines(ts(pred_cuad[,1], start = c(2023, 1), frequency = 12), 
      type = "l", col = "red", lty = 3, lwd = 2)


