 
# Forecast Models I
# ETS Models
# Forecast KO sales 
#(c)Ricardo A. Queralt
# 

require(forecast)
require(xts)
require(ggplot2)
library(ggfortify) #Plot Monthplot

# read data from CSV file
rawData <- read.csv("Data/ko.csv", sep = ";", dec = ",")

# Create a XTS object
# Convert data to XTS
mData = xts(rawData$Ingresos, order.by = as.Date(rawData$Fecha,"%Y%m%d"),frequency = 4)

xVentas = xts((rawData$Ingresos),order.by = as.POSIXct(strptime(rawData$Fecha,"%Y%m%d")),frequency = 4)

# Generate quarterly data
xVentas = to.quarterly(xVentas)  ## OJO cambia a que sea trimestral

#Transform to zoo data (forecast package)
zVentas = as.zoo(xVentas$xVentas.Close)  ## conviene cambiar de tipos por conveniencia de uso 
                                         ## de ciertas paqueterias

# Change name
names(zVentas) = "Ventas"


## Plot Serie
autoplot(zVentas) + ggtitle("Ventas Trimestrales CocaCola") + xlab("Trimestres") + ylab("Ventas")
# Seasonal Plot
ggfreqplot(as.ts(zVentas),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + 
  ggtitle("Ventas Trimestrales")

# Select number of observation to compare forecast
cOmit = 4

# Data Size
nObs = length(zVentas)

# sub_sample
# oVentas=zVentas[1:(nObs-cOmit),]
oVentas <- window(zVentas,start = index(zVentas[1]),end = index(zVentas[nObs - cOmit]))  
## Oventas serie de training

## Ejemplor de ajustes ####

#Fit Simple Exponential Smoothing
fit1 <- ses(oVentas)

#Fit Holt
fit2 <- holt(oVentas)

# Fit Holt- exponential
fit3 <- holt(oVentas,exponential = TRUE)

# Fit Holt - damped
fit4 <- holt(oVentas,damped = TRUE)

# Fit Holt - (exponential+damped)
fit5 <- holt(oVentas,exponential = TRUE,damped = )

# Results for first model:
fit1$model

# Plot models fitted
plot(fit3, type = "o", ylab = "Ventas",  flwd = 1, plot.conf = FALSE)
lines(window(zVentas),type = "o")
lines(fit1$mean,col = 2)
lines(fit2$mean,col = 3)
lines(fit4$mean,col = 5)
lines(fit5$mean,col = 6)
legend("topleft", lty = 1, pch = 1, col = 1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))



# seasonal model Holt-winters
fit6 <- hw(oVentas,seasonal = "additive")
fit7 <- hw(oVentas,seasonal = "multiplicative")

# Plot models
plot(fit7,ylab = "Ventas",
     plot.conf = FALSE, type = "o", fcol = "white", xlab = "Year")
lines(window(zVentas),type = "o",col = "blue")
lines(fitted(fit6), col = "red", lty = 2)
lines(fitted(fit7), col = "green", lty = 2)
lines(fit6$mean, type = "o", col = "red")
lines(fit7$mean, type = "o", col = "green")
legend("topleft",lty = 1, pch = 1, col = 1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))



## Calculate Components ####
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab = "Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean

## parece que funciona mejor el fit7


## Select automatic ETS ####
etsfit <- ets(oVentas)
#f orecast model
fventas.ets = forecast(etsfit)
# Results
summary(fventas.ets)  
## Eligio un MAM
## - M - multiplicativa la tendencia
## - A - aditiva en la pendiente
## - M - multiplicativa en estacionalidad


## ojo -> simpre que se hagan predicciones se debe dar un intervalo de confianza

# Plot
plot(fventas.ets)
lines(window(zVentas),type = "o")

#Actual and Forecast
matrix(c(fventas.ets$mean[1:cOmit],zVentas[(nObs - cOmit + 1):nObs]),ncol = 2)


## Select automatic ETS with chosen constraints ####
etsfit2 <- ets(oVentas,damped = TRUE)

## obligatoriamente amoriguado (dumped = TRUE)

#forecast model
fventas.ets2 = forecast(etsfit2)
#Results
summary(fventas.ets2)

#Plot
plot(fventas.ets2)
lines(window(zVentas),type = "o")

#Actual and Forecast
matrix(c(fventas.ets2$mean[1:cOmit], 
         fventas.ets$mean[1:cOmit],
         zVentas[(nObs - cOmit + 1):nObs]),ncol = 3)

#Plot all models
plot(fventas.ets2)
lines(window(zVentas),type = "o")
lines(fventas.ets$mean,type = "o",col = "red")

