########################################################################################################
## Start Date: 08/11/2019
## End Date:   -
## Author:     José María Álvarez Silva
## School:     CUNEF
## Class:      Predicción
## Assigment:  Primas Mapfre
## Language:   Spanish
##
########################################################################################################
## Predicción
########################################################################################################
## Primas Mapfre #######################################################################################
##     ETS y ARIMAS

## Propósito ###########################################################################################
##     Predicción del número de primas total, vida y no vida por separado para después sumar y comparar

## Paquetes ############################################################################################
##
library(dplyr)
library(tidyverse)
require(forecast)
require(xts)
require(ggplot2)
library(zoo)
library(hts)
library(ggfortify)
## Datos ###############################################################################################
##
datos <- read.csv("Primas_mapfre.csv", sep = ";", dec = ",")
## Total
datos <- datos %>% mutate(total = Primas_vida + Primas_no_vida)
## Fecha
datos$Fecha <- as.Date(datos$Fecha, "%m/%d/%Y")

## Análisis exploratorio de datos ######################################################################
##
ggplot(data = datos, aes(x = Fecha)) +
  geom_line(aes(y = Primas_vida, colour = "Vida")) +
  geom_line(aes(y = Primas_no_vida, colour = "No Vida")) +
  geom_line(aes(y = total, colour = "Total")) 

## Trabajando un modelo ETS para Total #################################################################
##

## Plot Serie Total
total = xts((datos$total), order.by = datos$Fecha, frequency = 4)
## Generate quarterly data
total = to.quarterly(total)  ## OJO cambia a que sea trimestral
## paqueteria zoo para mejor funcionamiento
total = as.zoo(total$total.Close) 
autoplot(total) + ggtitle("Primas Trimestrales") + xlab("Trimestres") + ylab("Primas")
## Seasonal Plot
ggfreqplot(as.ts(total), freq = 4, nrow = 1, facet.labeller = c("1T","2T","3T","4T")) + 
  ggtitle("Primas Trimestrales")

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast
cOmit = 4

## Data Size
nObs = length(total)

## sub_sample
oTotal <- window(total,start = index(total[1]),end = index(total[nObs - cOmit]))  

Totaletsfit <- ets(oTotal)
#f orecast model
fTotal.ets = forecast(Totaletsfit)
# Results
summary(fTotal.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(fTotal.ets)
lines(window(total),type = "o")

#Actual and Forecast
totalFitmat <- matrix(c(fTotal.ets$mean[1:cOmit],total[(nObs - cOmit + 1):nObs]),ncol = 2)
totalFitmat
## MSE
mean((totalFitmat[,1] - totalFitmat[,2])^2)
## MAE
mean(abs(totalFitmat[,1] - totalFitmat[,2]))
## Bias
mean(totalFitmat[,1] - totalFitmat[,2])


#- Complete set        -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select automatic ETS 
etsfit <- ets(total)
## forecast model
total.ets <- forecast(etsfit)
## Results
summary(total.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(total.ets, main = "Forcast Primas Mapfre")
lines(window(total),type = "o")


## Trabajando un modelo ETS por separado ###############################################################
##

## Plot Serie Primas Vida
vida = xts((datos$Primas_vida), order.by = datos$Fecha, frequency = 4)
## Generate quarterly data
vida = to.quarterly(vida)  ## OJO cambia a que sea trimestral
## paqueteria zoo para mejor funcionamiento
vida = as.zoo(vida$vida.Close) 
autoplot(vida) + ggtitle("Primas Trimestrales Vida") + xlab("Trimestres") + ylab("Ventas")
## Seasonal Plot
ggfreqplot(as.ts(vida), freq = 4, nrow = 1, facet.labeller = c("1T","2T","3T","4T")) + 
  ggtitle("Primas Trimestrales Vida")

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast
cOmit = 4

## Data Size
nObs = length(vida)

## sub_sample
ovida <- window(vida,start = index(vida[1]),end = index(vida[nObs - cOmit]))  

vidaetsfit <- ets(ovida)
#f orecast model
fvida.ets = forecast(vidaetsfit)
# Results
summary(fvida.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(fvida.ets)
lines(window(vida),type = "o")

#Actual and Forecast
vidaFitmat <- matrix(c(fvida.ets$mean[1:cOmit],vida[(nObs - cOmit + 1):nObs]),ncol = 2)
vidaFitmat
## MSE
mean((vidaFitmat[,1] - vidaFitmat[,2])^2)
## MAE
mean(abs(vidaFitmat[,1] - vidaFitmat[,2]))
## BIAS
mean((vidaFitmat[,1] - vidaFitmat[,2]))


#- Complete set        -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select automatic ETS 
etsfit <- ets(vida)
## forecast model
vida.ets <- forecast(etsfit)
## Results
summary(vida.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(vida.ets, main = "Forcast Primas vida")
lines(window(vida),type = "o")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Plot Serie Primas No Vida
no_vida <- xts((datos$Primas_no_vida), order.by = datos$Fecha, frequency = 4)
## Generate quarterly data
no_vida <- to.quarterly(no_vida)  ## OJO cambia a que sea trimestral
## paqueteria zoo para mejor funcionamiento
no_vida <- as.zoo(no_vida$no_vida.Close) 
autoplot(no_vida) + ggtitle("Primas Trimestrales No Vida") + xlab("Trimestres") + ylab("Primas")
## Seasonal Plot
ggfreqplot(as.ts(no_vida), freq = 4, nrow = 1, facet.labeller = c("1T","2T","3T","4T")) + 
  ggtitle("Primas Trimestrales No Vida")

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast
cOmit = 4

## Data Size
nObs = length(no_vida)

## sub_sample
ono_vida <- window(no_vida,start = index(no_vida[1]),end = index(no_vida[nObs - cOmit]))  

no_vidaetsfit <- ets(ono_vida)
#f orecast model
fno_vida.ets = forecast(no_vidaetsfit)
# Results
summary(fno_vida.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(fno_vida.ets)
lines(window(no_vida),type = "o")

#Actual and Forecast
no_vidaFitmat <- matrix(c(fno_vida.ets$mean[1:cOmit],no_vida[(nObs - cOmit + 1):nObs]),ncol = 2)
no_vidaFitmat
## MSE
mean((no_vidaFitmat[,1] - no_vidaFitmat[,2])^2)
## MAE
mean(abs(no_vidaFitmat[,1] - no_vidaFitmat[,2]))
## Bias
mean((no_vidaFitmat[,1] - no_vidaFitmat[,2]))

#- Complete set        -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select automatic ETS 
etsfit <- ets(no_vida)
## forecast model
no_vida.ets <- forecast(etsfit)
## Results
summary(no_vida.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad

## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(no_vida.ets, main = "Forcast Primas no vida")
lines(window(no_vida),type = "o")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## modelo ETS desde el enfoque de series de tiempo jerarquicas (hts)
##

## Plot Series
sepxts <- xts(datos[,c(3,4)], order.by = datos$Fecha, frequency = 4)
## Generate quarterly data
sepxtsVida   <- to.quarterly(sepxts$Primas_vida)     ## OJO cambia a que sea trimestral
sepxtsNoVida <- to.quarterly(sepxts$Primas_no_vida)  ## OJO cambia a que sea trimestral
## paqueteria zoo para mejor funcionamiento
sepxts <- cbind(sepxtsNoVida$`sepxts$Primas_no_vida.Close`, sepxtsVida$`sepxts$Primas_vida.Close`)
sepxts <- as.zoo(sepxts) 
names(sepxts) <- c("NV", "V")
autoplot(sepxts) + ggtitle("Primas Trimestrales") + xlab("Trimestres") + ylab("Primas")
## Select automatic HTS
sepmod <- hts(sepxts, nodes = list(2))
## Forcast
sep.fit <- forecast(sepmod, method = 'bu', fmethod = 'ets') # buttom up
names(sep.fit$labels) = c("Total", "No vida (NV) - Vida V")   
plot(sep.fit)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Despues de trabajar por separado vida y no vida sumamos para ver la prediccion total

#Actual and Forecast
sumaFitmat <- vidaFitmat + no_vidaFitmat
sumaFitmat
## MSE
mean((sumaFitmat[,1] - sumaFitmat[,2])^2)
## MAE
mean(abs(sumaFitmat[,1] - sumaFitmat[,2]))
## Bias
mean(sumaFitmat[,1] - sumaFitmat[,2])

  
## Trabajando un modelo ARIMA para Total #############################################################
##

## Nuestra ts de primas totales se llama "total"

df_total <- data.frame(value = as.vector(total),
                       time = time(total))
ggplot(df_total) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("Primas") + 
  ggtitle("Primas Trimestrales Mapfre") + 
  xlab("Trimestres")

## trabajamos con transformacion logaritmica
logTotal <- log(total)
df_logTotal <- data.frame(value = as.vector(logTotal),
                      time = time(logTotal))
ggplot(df_logTotal) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("log - Primas") + 
  ggtitle("Primas Trimestrales Mapfre (logarítmicas)") + 
  xlab("Trimestres")

## Difference
ggtsdisplay(logTotal)
ggtsdisplay(diff(logTotal))
ggtsdisplay(diff(logTotal,4))
ggtsdisplay(diff(diff(logTotal,4),1))

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#Select number of observation to compare forecast
cOmit = 4

#Data Size
nObs = length(total)

#sub_sample TRAINING
oatotal <- window(total, start = index(total[1]), end = index(total[nObs - cOmit]))

## ARIMA MODEL Automatic selection####
total.train.arima = auto.arima(oatotal,lambda = 0) ## lamnda cero is log transformation
summary(total.train.arima)

#residual analysis
ggtsdisplay(total.train.arima$residuals)

#box-Ljung Test
Box.test(total.train.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(total.train.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(total.train.arima$residuals,lag = 12, fitdf = 3, type = "Lj")

plot(forecast(total.train.arima))
lines(window(total),type = "o")

ftotal_arima <- forecast(total.train.arima)

totalArimaMatrix <- matrix(c(ftotal_arima$mean[1:4], as.double(tail(total,4))), ncol = 2)
totalArimaMatrix
## MSE
mean((totalArimaMatrix[,1] - totalArimaMatrix[,2])^2)
## MAE
mean(abs(totalArimaMatrix[,1] - totalArimaMatrix[,2]))
## Bias
mean(totalArimaMatrix[,1] - totalArimaMatrix[,2])

#- Complete set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## ARIMA MODEL Automatic selection
total.fit.arima <- auto.arima(total, lambda = 0) ## lamnda cero para transformacion log 
summary(total.fit.arima)

#residual analysis
ggtsdisplay(total.fit.arima$residuals)

#box-Ljung Test
Box.test(total.fit.arima$residuals, lag = 4, fitdf = 3, type = "Lj")
Box.test(total.fit.arima$residuals, lag = 8, fitdf = 3, type = "Lj")
Box.test(total.fit.arima$residuals, lag = 12, fitdf = 3, type = "Lj")

tota.arima <- forecast(total.fit.arima)

ggplot(df_total) + geom_point(aes(x = time,y = value)) + 
  geom_line(aes(x = time, y = value)) +
  geom_forecast(tota.arima, alpha = 0.4) +
  ggtitle("ARIMA: Predicción Primas")

## Trabajando un modelo ARIMA para Primas por separdo #################################################
##

## Nuestra ts de primas vida se llama "vida"

df_vida <- data.frame(value = as.vector(vida),
                       time = time(vida))
ggplot(df_vida) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("Primas") + 
  ggtitle("Primas Trimestrales Mapfre") + 
  xlab("Trimestres")

## trabajamos con transformacion logaritmica
logvida <- log(vida)
df_logvida <- data.frame(value = as.vector(logvida),
                          time = time(logvida))
ggplot(df_logvida) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("log - Primas") + 
  ggtitle("Primas Trimestrales Mapfre (logarítmicas)") + 
  xlab("Trimestres")

## Difference
ggtsdisplay(logvida)
ggtsdisplay(diff(logvida))
ggtsdisplay(diff(logvida,4))
ggtsdisplay(diff(diff(logvida,4),1))

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#Select number of observation to compare forecast
cOmit = 4

#Data Size
nObs = length(vida)

#sub_sample TRAINING
oavida <- window(vida, start = index(vida[1]), end = index(vida[nObs - cOmit]))

## ARIMA MODEL Automatic selection####
vida.train.arima = auto.arima(oavida,lambda = 0) ## lamnda cero is log transformation
summary(vida.train.arima)

#residual analysis
ggtsdisplay(vida.train.arima$residuals)

#box-Ljung Test
Box.test(vida.train.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(vida.train.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(vida.train.arima$residuals,lag = 12, fitdf = 3, type = "Lj")

plot(forecast(vida.train.arima))
lines(window(vida),type = "o")

fvida_arima <- forecast(vida.train.arima)

vidaArimaMatrix <- matrix(c(fvida_arima$mean[1:4], as.double(tail(vida,4))), ncol = 2)
vidaArimaMatrix

## MSE
mean((vidaArimaMatrix[,1] - vidaArimaMatrix[,2])^2)
## MAE
mean(abs(vidaArimaMatrix[,1] - vidaArimaMatrix[,2]))
## Bias
mean(vidaArimaMatrix[,1] - vidaArimaMatrix[,2])

#- Complete set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## ARIMA MODEL Automatic selection
vida.fit.arima <- auto.arima(vida, lambda = 0) ## lamnda cero para transformacion log 
summary(vida.fit.arima)

#residual analysis
ggtsdisplay(vida.fit.arima$residuals)

#box-Ljung Test
Box.test(vida.fit.arima$residuals, lag = 4, fitdf = 3, type = "Lj")
Box.test(vida.fit.arima$residuals, lag = 8, fitdf = 3, type = "Lj")
Box.test(vida.fit.arima$residuals, lag = 12, fitdf = 3, type = "Lj")

vida.arima <- forecast(vida.fit.arima)

ggplot(df_vida) + geom_point(aes(x = time,y = value)) + 
  geom_line(aes(x = time, y = value)) +
  geom_forecast(vida.arima, alpha = 0.4) +
  ggtitle("ARIMA: Predicción Primas Vida")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Nuestra ts de primas no vida se llama "no_vida"

df_no_vida <- data.frame(value = as.vector(no_vida),
                      time = time(no_vida))
ggplot(df_no_vida) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("Primas") + 
  ggtitle("Primas Trimestrales Mapfre") + 
  xlab("Trimestres")

## trabajamos con transformacion logaritmica
logno_vida <- log(no_vida)
df_logno_vida <- data.frame(value = as.vector(logno_vida),
                         time = time(logno_vida))
ggplot(df_logno_vida) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("log - Primas") + 
  ggtitle("Primas Trimestrales Mapfre (logarítmicas)") + 
  xlab("Trimestres")

## Difference
ggtsdisplay(logno_vida)
ggtsdisplay(diff(logno_vida))
ggtsdisplay(diff(logno_vida,4))
ggtsdisplay(diff(diff(logno_vida,4),1))


#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#Select number of observation to compare forecast
cOmit = 4

#Data Size
nObs = length(no_vida)

#sub_sample TRAINING
oano_vida <- window(no_vida, start = index(no_vida[1]), end = index(no_vida[nObs - cOmit]))

## ARIMA MODEL Automatic selection####
no_vida.train.arima = auto.arima(oano_vida,lambda = 0) ## lamnda cero is log transformation
summary(no_vida.train.arima)

#residual analysis
ggtsdisplay(no_vida.train.arima$residuals)

#box-Ljung Test
Box.test(no_vida.train.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(no_vida.train.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(no_vida.train.arima$residuals,lag = 12, fitdf = 3, type = "Lj")

plot(forecast(no_vida.train.arima))
lines(window(no_vida),type = "o")

fno_vida_arima <- forecast(no_vida.train.arima)

no_vidaArimaMatrix <- matrix(c(fno_vida_arima$mean[1:4], as.double(tail(no_vida,4))), ncol = 2)
no_vidaArimaMatrix

## MSE
mean((no_vidaArimaMatrix[,1] - no_vidaArimaMatrix[,2])^2)
## MAE
mean(abs(no_vidaArimaMatrix[,1] - no_vidaArimaMatrix[,2]))
## Bias
mean(no_vidaArimaMatrix[,1] - no_vidaArimaMatrix[,2])

#- Complete set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


## ARIMA MODEL Automatic selection
no_vida.fit.arima <- auto.arima(no_vida, lambda = 0) ## lamnda cero para transformacion log 
summary(no_vida.fit.arima)

#residual analysis
ggtsdisplay(no_vida.fit.arima$residuals)

#box-Ljung Test
Box.test(no_vida.fit.arima$residuals, lag = 4, fitdf = 3, type = "Lj")
Box.test(no_vida.fit.arima$residuals, lag = 8, fitdf = 3, type = "Lj")
Box.test(no_vida.fit.arima$residuals, lag = 12, fitdf = 3, type = "Lj")

no_vida.arima <- forecast(no_vida.fit.arima)

ggplot(df_no_vida) + geom_point(aes(x = time,y = value)) + 
  geom_line(aes(x = time, y = value)) +
  geom_forecast(no_vida.arima, alpha = 0.4) +
  ggtitle("ARIMA: Predicción Primas No Vida")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## modelo ARIMA desde el enfoque de series de tiempo jerarquicas (hts)
##

## Plot Series
autoplot(sepxts) + ggtitle("Primas Trimestrales") + xlab("Trimestres") + ylab("Primas")
## Select automatic HTS
sepmod <- hts(sepxts, nodes = list(2))
## Forcast
sep.fit.arima <- forecast(sepmod, method = 'bu', fmethod = 'arima') ## buttom up arima
names(sep.fit.arima$labels) = c("Total", "No vida (NV) - Vida V")   
plot(sep.fit.arima)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Despues de trabajar por separado vida y no vida sumamos para ver la prediccion total

sumaFitArimaMat <- vidaArimaMatrix + no_vidaArimaMatrix
sumaFitArimaMat
## MSE
mean((sumaFitArimaMat[,1] - sumaFitArimaMat[,2])^2)
## MAE
mean(abs(sumaFitArimaMat[,1] - sumaFitArimaMat[,2]))
## Bias
mean(sumaFitArimaMat[,1] - sumaFitArimaMat[,2])


## Conclusiones ######################################################################################
##

## Comparamos los forcast de los modelos
compmat <-  matrix(c(as.vector(total.ets$mean), 
                     as.vector(vida.ets$mean),
                     as.vector(no_vida.ets$mean),
                     as.vector(vida.ets$mean) + as.vector(no_vida.ets$mean),
                     as.vector(sep.fit$bts[,2]),
                     as.vector(sep.fit$bts[,1]),
                     as.vector(sep.fit$bts[,1]) + as.vector(sep.fit$bts[,2]),
                     as.vector(tota.arima$mean), 
                     as.vector(vida.arima$mean),
                     as.vector(no_vida.arima$mean),
                     as.vector(vida.arima$mean) + as.vector(no_vida.arima$mean),
                     as.vector(sep.fit.arima$bts[,2]),
                     as.vector(sep.fit.arima$bts[,1]),
                     as.vector(sep.fit.arima$bts[,1]) + as.vector(sep.fit.arima$bts[,2]),
                     as.vector(time(total.ets$mean))),ncol = 15)

colnames(compmat) <- c("Total ETS",
                       "Vida ETS",
                       "No Vida ETS",
                       "V+NV ETS",
                       "Vida ETS HTS",
                       "No Vida ETS HTS",
                       "V+NV ETS HTS",
                       "Total ARIMA",
                       "Vida ARIMA",
                       "No Vida ARIMA",
                       "V+NV ARIMA",
                       "Vida ARIMA HTS",
                       "No Vida ARIMA HTS",
                       "V+NV ARIMA HTS",
                       "Trimestre")

compmat <- as.data.frame(compmat)
compmat

## comparamos graficamente
ggplot(data = compmat, aes(x = Trimestre)) +
  geom_line(aes(y = `Total ETS`, colour = "Total ETS")) +
  geom_line(aes(y = `V+NV ETS`, colour = "V+NV ETS")) +
  geom_line(aes(y = `Total ARIMA`, colour = "Total ARIMA")) +
  geom_line(aes(y = `V+NV ARIMA`, colour = "V+NV ARIMA")) +
  ggtitle("Primas Totales") + ylab("Primas")

ggplot(data = compmat, aes(x = Trimestre)) +
  geom_line(aes(y = `Vida ETS`, colour = "Vida ETS")) +
  geom_line(aes(y = `Vida ARIMA`, colour = "Vida ARIMA")) +
  ggtitle("Primas Vida") + ylab("Primas")

ggplot(data = compmat, aes(x = Trimestre)) +
  geom_line(aes(y = `No Vida ETS`, colour = "No Vida ETS")) +
  geom_line(aes(y = `No Vida ARIMA`, colour = "No Vida ARIMA")) +
  ggtitle("Primas No Vida") + ylab("Primas")
