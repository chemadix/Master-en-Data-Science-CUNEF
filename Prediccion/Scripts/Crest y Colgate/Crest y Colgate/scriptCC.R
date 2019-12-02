########################################################################################################
## Start Date: 18/11/2019
## End Date:   -
## Author:     José María Álvarez Silva
## School:     CUNEF
## Class:      Predicción
## Assigment:  Crest y Colgate
## Language:   Spanish
##
########################################################################################################
## Predicción
########################################################################################################
## Primas Mapfre #######################################################################################
##     ARIMAX y ARIMAS

## Propósito ###########################################################################################
##     Predicción del MS de Crest y Colgate con intervencion y sin intervención.

## Paquetes ############################################################################################
##
library(dplyr)
library(tidyverse)
library(forecast)
library(xts)
library(ggplot2)
library(zoo)
library(ggfortify)
library(skimr)
library(gridExtra)
library(ggpubr)
library(TSA)
library(Hmisc)
library(astsa)
library(dynlm)

## Datos ###############################################################################################
##
datos <- read.csv("data.csv")

datos$Date <- as.Date(paste(datos$Year, datos$Week, 1, sep = "-"), "%Y-%U-%u")

skim(datos)
## Análisis Exploratorio de Datos ######################################################################
##

## Series 
ggplot(data = datos, aes(x = Date)) +
  geom_line(aes(y = Crest, colour = "Crest")) +
  geom_line(aes(y = Colgate, colour = "Colgate")) +
  ylab("Market Share")

ggplot(data = filter(datos, Year == 1959 | Year == 1960 | Year == 1961), aes(x = Date)) +
  geom_line(aes(y = Crest, colour = "Crest")) +
  geom_line(aes(y = Colgate, colour = "Colgate")) +
  ylab("Market Share") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 2) ## 1 agosto 1960

#-#- Crest #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Plot Serie crest
crest = xts((datos$Crest), order.by = datos$Date)
colnames(crest) <- "Crest"
## paqueteria zoo para mejor funcionamiento
crest = as.zoo(crest$Crest) 
autoplot(crest) + ggtitle("Market Share Semanal - Crest") + xlab("Semanas") + ylab("Market Share") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 2)

## Nuestra ts de market share de Crest de llama Crest

df_crest <- data.frame(value = as.vector(crest),
                       time = time(crest))
ggplot(df_crest) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("Market Share") + 
  ggtitle("Market Share Semanal- Crest") + 
  xlab("Semanas") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 2)

## trabajamos con transformacion logaritmica
logcrest <- log(crest)
df_logcrest <- data.frame(value = as.vector(logcrest),
                          time = time(logcrest))
ggplot(df_logcrest) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("log - MktShare") + 
  ggtitle("Market Share Semanal- Crest (logarítmico)") + 
  xlab("Semanas")

## Difference
ggtsdisplay(logcrest)
ggtsdisplay(diff(logcrest))

which(diff(logcrest) == max(diff(logcrest)))
which(diff(logcrest) == min(diff(logcrest)))

#-#- Colgate #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

## Plot Serie colgate
colgate = xts((datos$Colgate), order.by = datos$Date)
colnames(colgate) <- "colgate"
## paqueteria zoo para mejor funcionamiento
colgate = as.zoo(colgate$colgate) 
autoplot(colgate) + ggtitle("Market Share Semanal - Colgate") + xlab("Semanas") + ylab("Market Share") + 
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 2)

## Nuestra ts de market share de colgate de llama colgate

df_colgate <- data.frame(value = as.vector(colgate),
                       time = time(colgate))
ggplot(df_colgate) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("Market Share") + 
  ggtitle("Market Share Semanal- Colgate") + 
  xlab("Semanas")

## trabajamos con transformacion logaritmica
logcolgate <- log(colgate)
df_logcolgate <- data.frame(value = as.vector(logcolgate),
                            time = time(logcolgate))
ggplot(df_logcolgate) + geom_point(aes(x = time, y = value)) + 
  geom_line(aes(x = time, y = value)) + 
  ylab("log - MktShare") + 
  ggtitle("Market Share Semanal- Colgate (logarítmico)") + 
  xlab("Semanas")

## Difference
ggtsdisplay(logcolgate)
ggtsdisplay(diff(logcolgate))
ggtsdisplay(diff(logcolgate, 12))

which(diff(logcolgate) == max(diff(logcolgate)))
which(diff(logcolgate) == min(diff(logcolgate)))

#-#- Fechas Importantes #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


## histograms


p1 <- ggplot(data = diff(logcrest,12), aes(Crest)) +
  geom_histogram(aes(y = ..density.., fill = ..count..), color = "white") +
  xlab("Crest") +
  geom_density(fill = "steelblue", alpha = 0.5, color = "white") + 
  theme(legend.position = "None")  + ylim(0,2) + xlim(-1.5,1.5) +
  geom_vline(xintercept = c(2 * sd(diff(logcrest,12)),-2 * sd(diff(logcrest,12))),
             linetype = 2, colour = "red")
p2 <- ggplot(data = diff(logcolgate,12), aes(colgate)) +
  geom_histogram(aes(y = ..density.., fill = ..count..), color = "white") +
  xlab("Colgate") +
  geom_density(fill = "steelblue", alpha = 0.5, color = "white") + 
  theme(legend.position = "None")  + ylim(0,2) + xlim(-1.5,1.5) +
  geom_vline(xintercept = c(2 * sd(diff(logcolgate,12)),-2 * sd(diff(logcolgate,12))),
             linetype = 2, colour = "red")

grid.arrange(
  p1, p2,
  widths = c( 1, 1),
  top = text_grob("LogDifferences"),
  layout_matrix = rbind(c(1, 2),
                        c(1, 2))
)

k = 4

which(abs(diff(logcrest,12)) > k*sd(abs(diff(logcrest,12))))
which(abs(diff(logcolgate,12)) > k*sd(abs(diff(logcolgate,12))))

## trabajando con logaritmos

p1 <- autoplot(((logcrest)))  +
  geom_vline(xintercept = as.numeric(datos$Date[126 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[126 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[90 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[22 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 1, colour = "blue")
p2 <- autoplot(((logcolgate))) +
  geom_vline(xintercept = as.numeric(datos$Date[90 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[102 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[187 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[199 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 1, colour = "blue")

grid.arrange(
  p1, p2,
  widths = c( 1, 1),
  #top = text_grob(Character),
  layout_matrix = rbind(c(1, 1),
                        c(2, 2))
)


## 12 lags (trimestre)

p1 <- autoplot((diff(logcrest,12)))  +
  geom_vline(xintercept = as.numeric(datos$Date[126 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[126 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[90 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[22 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 1, colour = "blue")
p2 <- autoplot((diff(logcolgate,12))) +
  geom_vline(xintercept = as.numeric(datos$Date[90 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[102 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[187 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[199 + 12]), linetype = 2, colour = "red") +
  geom_vline(xintercept = as.numeric(datos$Date[135]), linetype = 1, colour = "blue")

grid.arrange(
  p1, p2,
  widths = c( 1, 1),
  #top = text_grob(Character),
  layout_matrix = rbind(c(1, 1),
                        c(2, 2))
)


## Auto Arima #########################################################################################
##

## Crest ARIMA ########################################################################################
modCrest <- auto.arima(logcrest)
summary(modCrest)

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast (16 semanas)
cOmit = 16

## Data Size
nObs = length(logcrest)

## sub_sample
oCrest <- window(logcrest,start = index(logcrest[1]),end = index(logcrest[nObs - cOmit]))  

## ARIMA MODEL Automatic selection####
crest.train.arima = auto.arima(oCrest) ## lamnda cero is log transformation
summary(crest.train.arima)

#residual analysis
ggtsdisplay(crest.train.arima$residuals)

#box-Ljung Test
Box.test(crest.train.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(crest.train.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(crest.train.arima$residuals,lag = 12, fitdf = 3, type = "Lj")
## Residuales independientes

plot(forecast(crest.train.arima, h = 16))
lines(window(logcrest),type = "o")

plot(forecast(crest.train.arima, h = 16), xlim = c(-2800, -2420), ylim = c(-2,0))
lines(window(logcrest),type = "o")

fcrest_arima <- forecast(crest.train.arima, h = 16) ## predecimos 16 semanas

crestArimaMatrix <- matrix(c(fcrest_arima$mean[1:16], as.double(tail(logcrest,16))), ncol = 2)
crestArimaMatrix
## MSE
mean((crestArimaMatrix[,1] - crestArimaMatrix[,2])^2)
## MAE
mean(abs(crestArimaMatrix[,1] - crestArimaMatrix[,2]))
## Bias
mean(crestArimaMatrix[,1] - crestArimaMatrix[,2])

## Colgate ARIMA ######################################################################################
modColgate <- auto.arima(logcolgate)
summary(modColgate)

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast (16 semanas)
cOmit = 16

## Data Size
nObs = length(logcolgate)

## sub_sample
ocolgate <- window(logcolgate,start = index(logcolgate[1]),end = index(logcolgate[nObs - cOmit]))  

## ARIMA MODEL Automatic selection####
colgate.train.arima = auto.arima(ocolgate) ## lamnda cero is log transformation
summary(colgate.train.arima)

#residual analysis
ggtsdisplay(colgate.train.arima$residuals)

#box-Ljung Test
Box.test(colgate.train.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(colgate.train.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(colgate.train.arima$residuals,lag = 12, fitdf = 3, type = "Lj")
## Residuales independientes

plot(forecast(colgate.train.arima, h = 16))
lines(window(logcolgate),type = "o")

plot(forecast(colgate.train.arima, h = 16), xlim = c(-2800, -2420), ylim = c(-2,0))
lines(window(logcolgate),type = "o")

fcolgate_arima <- forecast(colgate.train.arima, h = 16) ## predecimos 16 semanas

colgateArimaMatrix <- matrix(c(fcolgate_arima$mean[1:16], as.double(tail(logcolgate,16))), ncol = 2)
colgateArimaMatrix
## MSE
mean((colgateArimaMatrix[,1] - colgateArimaMatrix[,2])^2)
## MAE
mean(abs(colgateArimaMatrix[,1] - colgateArimaMatrix[,2]))
## Bias
mean(colgateArimaMatrix[,1] - colgateArimaMatrix[,2])

## Outliers ###########################################################################################
##

## Crest
detectAO(modCrest)
detectIO(modCrest)

## Colgate
detectAO(modColgate)
detectIO(modColgate)

## ARIMAX #############################################################################################
##

## Crest ARIMAX #######################################################################################

pulseFCrest <- data.frame(ADA = 1*(seq(logcrest) == which(datos$Date == "1960-08-01")))[,1]
plot(pulseFCrest)

stepFCrest <- data.frame(ADA = 1*(seq(logcolgate) > which(datos$Date == "1960-08-01")))[,1]
plot(stepFCrest)

df_crest <- data.frame(pulseF,stepF)

## Solo step
crest.m1 = arimax(as.double(logcrest$Crest),
                  order = c(3,1,0), method = 'ML',
                  xtransf = data.frame(ADA = stepFCrest),
                  transfer = list(c(0,0)),
                  xreg = data.frame(Imp1 = 1*(seq(logcolgate) == (22 + 12)),
                                    Imp2 = 1*(seq(logcolgate) == (90 + 12)))
                  )
crest.m1

plot(ts(stepFCrest*(0.5808)))

plot(as.double(logcrest), ylab = "Log(Crest)", type = "l")
points(fitted(crest.m1), col = "blue")

#- Training set     -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Select number of observation to compare forecast (16 semanas)
cOmit = 16

## Data Size
nObs = length(logcrest)

## sub_sample
oCrest <- window(logcrest,start = index(logcrest[1]),end = index(logcrest[nObs - cOmit]))

stepFCrestARIMAX <- data.frame(ADA = 1*(seq(oCrest) > which(datos$Date == "1960-08-01")))[,1]

## ARIMAX MODEL CREST 
crest.train.arimax = arimax(as.double(oCrest$Crest),
                            order = c(3,1,0), method = 'ML',
                            xtransf = data.frame(ADA = stepFCrestARIMAX),
                            transfer = list(c(0,0))#,
                            #xreg = data.frame(Imp1 = 1*(seq(oCrest) == (22 + 12)),
                                             # Imp2 = 1*(seq(oCrest) == (90 + 12)))
)
summary(crest.train.arimax)

#residual analysis
ggtsdisplay(crest.train.arimax$residuals)

#box-Ljung Test
Box.test(crest.train.arimax$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(crest.train.arimax$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(crest.train.arimax$residuals,lag = 12, fitdf = 3, type = "Lj")
## Residuales independientes

plot(forecast(crest.train.arimax), h = 16)
lines(window(logcrest),type = "o")

plot(forecast(crest.train.arimax, h = 16), xlim = c(-2800, -2420), ylim = c(-2,0))
lines(window(logcrest),type = "o")

fcrest_arimax <- forecast(crest.train.arimax, h = 16) ## predecimos 16 semanas

crestArimaxMatrix <- matrix(c(fcrest_arimax$mean[1:16], as.double(tail(logcrest,16))), ncol = 2)
crestArimaxMatrix
## MSE
mean((crestArimaxMatrix[,1] - crestArimaxMatrix[,2])^2)
## MAE
mean(abs(crestArimaxMatrix[,1] - crestArimaxMatrix[,2]))
## Bias
mean(crestArimaxMatrix[,1] - crestArimaxMatrix[,2])

## Colgate ARIMAX #####################################################################################

pulseFColgate <- data.frame(ADA = 1*(seq(logcolgate) == which(datos$Date == "1960-08-01")))[,1]
plot(pulseFColgate)

stepFColgate <- data.frame(ADA = 1*(seq(logcolgate) > which(datos$Date == "1960-08-01")))[,1]
plot(stepFColgate)

dfcolgate <- data.frame(pulseFColgate,stepFColgate)

colgate.m1 = arimax(as.double(colgate$colgate),
                  order = c(0,1,1), method = 'ML',
                  xtransf = dfcolgate,
                  transfer = list(c(2,0),c(0,0)))
colgate.m1

## Solo step
colgate.m1 = arimax(as.double(logcolgate$colgate),
                    order = c(0,1,1), method = 'ML',
                    xtransf = data.frame(ADA = stepFColgate),
                    transfer = list(c(0,0)),
                    xreg = data.frame(Imp1 = 1*(seq(logcolgate) == (90 + 12)),
                                      Imp2 = 1*(seq(logcolgate) == (187 + 12)))
                    )
colgate.m1

plot(ts(stepFCrest*(-0.3064)))

plot(as.double(logcolgate), ylab = "Log(Colgate)", type = "l")
points(fitted(colgate.m1), col = "blue")

## Regresión dinámica y función de transferencia ######################################################
##

modDyn <- dynlm(logcrest ~ L(logcrest, 1) + L(logcolgate, 0:12)) 
modDyn

tsdisplay(modDyn$residuals)

## Diff

modDDyn <- dynlm(diff(logcrest) ~ L(diff(logcrest), 1) + L(diff(logcolgate), 0:12)) 
modDDyn

tsdisplay(modDDyn$residuals)

### ARIMAX

plot(modDA$coef[4:15], type = "h")

modDA <- arimax(as.double(diff(logcrest)),
                order = c(3,1,0),
                include.mean = T,
                xtransf = as.double(diff(logcolgate)),
                transfer = list(c(0,12)),
                method = "ML")
modDA

tsdisplay(modDA$residuals)

plot(modDA$coef[4:16], type = "h")

modDA <- arimax(as.double(diff(logcrest)),
                order = c(3,1,0),
                include.mean = T,
                xtransf = as.double(diff(logcolgate)),
                transfer = list(c(0,0)),
                method = "ML")
modDA
tsdisplay(modDA$residuals)
